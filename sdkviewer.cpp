#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <vector>
#include <string>
#include <fstream>
#include <sstream>
#include <regex>
#include <map>
#include <filesystem>
#include <algorithm>
#include <thread>
#include <atomic>
#include <mutex>
#include <future>
#include <set>
#include <functional>
#include <queue>
#include <unordered_map>

#include "imgui/imgui.h"
#include "imgui/imgui_impl_win32.h"
#include "imgui/imgui_impl_dx11.h"
#include <d3d11.h>
#include <tchar.h>

#pragma comment(lib, "d3d11.lib")

namespace fs = std::filesystem;

// ---------------------------------------------------------
// DATA STRUCTURES
// ---------------------------------------------------------

struct SDKMember {
    std::string Type;
    std::string Name;
    std::string OffsetStr;
    uint64_t OffsetVal = 0;
    std::string Size;
    std::string Flags;
    bool bIsBitfield = false;
    int LinkedClassIdx = -1;
    int LinkedEnumIdx = -1;
    int LinkedTypeIdx = -1;
    std::string CleanTypeName;
};

struct SDKFunction {
    std::string Name;
    std::string ReturnType;
    std::string Params;
    std::string Flags;
};

struct SDKClass {
    std::string Name;
    std::string FullName;
    std::string ParentName;
    std::string SourceFile;
    bool bIsStruct = false;
    std::vector<SDKMember> Members;
    std::vector<SDKFunction> Functions;
    int ParentClassIdx = -1;
};

struct SDKEnumMember {
    std::string Name;
    std::string ValueStr;
    int64_t Value = 0;
};

struct SDKEnum {
    std::string Name;
    std::string FullName;
    std::string Type;
    std::string SourceFile;
    std::vector<SDKEnumMember> Members;
};

struct SDKTypeDef {
    std::string Alias;
    std::string Underlying;
    std::string SourceFile;
};

struct GlobalSearchResult {
    enum ResultType { CLASS, STRUCT, ENUM, TYPEDEF, FUNCTION, PROPERTY };
    ResultType Type;
    int ContainerIdx;
    int MemberIdx;
    std::string Name;
    std::string Context;
    std::string ExtraInfo;
};

struct OffsetChainLink {
    std::string memberName;
    uint64_t offset;
    uint64_t totalOffset;
    std::string typeName;
    std::string containerName;
};

// ---------------------------------------------------------
// HELPER UTILS
// ---------------------------------------------------------

uint64_t ParseHex(const std::string& str) {
    try { return std::stoull(str, nullptr, 16); }
    catch (...) { return 0; }
}

bool StringContains(const std::string& haystack, const std::string& needle) {
    if (needle.empty()) return true;
    auto it = std::search(
        haystack.begin(), haystack.end(),
        needle.begin(), needle.end(),
        [](char ch1, char ch2) { return std::toupper(ch1) == std::toupper(ch2); }
    );
    return (it != haystack.end());
}

void TrimString(std::string& s) {
    if (s.empty()) return;
    s.erase(0, s.find_first_not_of(" \t\r\n"));
    s.erase(s.find_last_not_of(" \t\r\n") + 1);
}

// ---------------------------------------------------------
// NAVIGATION HISTORY
// ---------------------------------------------------------

struct NavHistory {
    struct Entry {
        int TabIndex;
        int ClassIdx;
        int EnumIdx;
        int TypeDefIdx;
    };
    std::vector<Entry> History;
    int CurrentIndex = -1;

    void Push(int Tab, int CIdx, int EIdx, int TIdx) {
        if (CurrentIndex < (int)History.size() - 1) {
            History.resize(CurrentIndex + 1);
        }
        History.push_back({ Tab, CIdx, EIdx, TIdx });
        CurrentIndex++;
    }

    Entry GoBack() {
        if (CurrentIndex > 0) {
            CurrentIndex--;
            return History[CurrentIndex];
        }
        return { -1, -1, -1, -1 };
    }

    void Clear() {
        History.clear();
        CurrentIndex = -1;
    }
};

// ---------------------------------------------------------
// PARSER ENGINE
// ---------------------------------------------------------

class SDKParser {
public:
    std::vector<SDKClass> Classes;
    std::vector<SDKEnum> Enums;
    std::vector<SDKTypeDef> TypeDefs;

    std::vector<int> FilteredClassIndices;
    std::vector<int> FilteredEnumIndices;
    std::vector<int> FilteredTypeDefIndices;

    std::vector<GlobalSearchResult> GlobalResults;

    std::map<std::string, int> ClassLookup;
    std::map<std::string, int> EnumLookup;
    std::map<std::string, int> TypeDefLookup;
    std::map<int, std::vector<int>> DerivedLookup;

    // REVERSE MAP: Key = Child Class Index, Value = List of Parents (Indices) that point to it
    struct ReverseLink {
        int ParentClassIdx;
        std::string MemberName;
        uint64_t Offset;
    };
    std::unordered_map<int, std::vector<ReverseLink>> ReverseReferenceMap;

    std::atomic<bool> IsLoading = false;
    std::atomic<int> FilesProcessed = 0;
    std::atomic<int> TotalFiles = 0;
    std::string LoadingStatus = "Idle";

    void Clear() {
        Classes.clear(); Enums.clear(); TypeDefs.clear();
        ClassLookup.clear(); EnumLookup.clear(); TypeDefLookup.clear(); DerivedLookup.clear();
        ReverseReferenceMap.clear();
        FilteredClassIndices.clear(); FilteredEnumIndices.clear(); FilteredTypeDefIndices.clear();
        GlobalResults.clear();
        FilesProcessed = 0;
    }

    std::string GenerateClassDump(int classIdx) {
        if (classIdx < 0 || classIdx >= Classes.size()) return "";
        const auto& cls = Classes[classIdx];
        std::stringstream ss;
        ss << "// " << cls.FullName << "\n";
        ss << "// File: " << cls.SourceFile << "\n";
        ss << (cls.bIsStruct ? "struct " : "class ") << cls.Name;
        if (!cls.ParentName.empty()) ss << " : public " << cls.ParentName;
        ss << "\n{\npublic:\n";
        for (const auto& m : cls.Members) {
            ss << "\t" << m.Type << " " << m.Name << "; // " << m.OffsetStr << "(" << m.Size << ")";
            if (!m.Flags.empty()) ss << "(" << m.Flags << ")";
            ss << "\n";
        }
        if (!cls.Functions.empty()) {
            ss << "\npublic:\n";
            for (const auto& f : cls.Functions) {
                ss << "\t" << f.ReturnType << " " << f.Name << f.Params << "; // " << f.Flags << "\n";
            }
        }
        ss << "};\n";
        return ss.str();
    }

    std::string CleanType(std::string rawType) {
        size_t p;
        if ((p = rawType.find("const ")) != std::string::npos) rawType.erase(p, 6);
        while ((p = rawType.find("class ")) != std::string::npos) rawType.erase(p, 6);
        while ((p = rawType.find("struct ")) != std::string::npos) rawType.erase(p, 7);
        while ((p = rawType.find("enum ")) != std::string::npos) rawType.erase(p, 5);
        if ((p = rawType.find("SDK::")) != std::string::npos) rawType.erase(p, 5);
        if ((p = rawType.find("UC::")) != std::string::npos) rawType.erase(p, 4);

        rawType.erase(std::remove(rawType.begin(), rawType.end(), '*'), rawType.end());
        rawType.erase(std::remove(rawType.begin(), rawType.end(), '&'), rawType.end());

        size_t angleOpen = rawType.find('<');
        size_t angleClose = rawType.find_last_of('>');
        if (angleOpen != std::string::npos && angleClose != std::string::npos && angleClose > angleOpen) {
            std::string inner = rawType.substr(angleOpen + 1, angleClose - angleOpen - 1);
            return CleanType(inner);
        }
        TrimString(rawType);
        return rawType;
    }

    void PerformGlobalSearch(const std::string& query) {
        GlobalResults.clear();
        if (query.length() < 2) return;

        for (int i = 0; i < Classes.size(); i++) {
            if (StringContains(Classes[i].Name, query)) {
                GlobalResults.push_back({ Classes[i].bIsStruct ? GlobalSearchResult::STRUCT : GlobalSearchResult::CLASS, i, -1, Classes[i].Name, Classes[i].Name, Classes[i].SourceFile });
            }
            for (int f = 0; f < Classes[i].Functions.size(); f++) {
                if (StringContains(Classes[i].Functions[f].Name, query)) {
                    GlobalResults.push_back({ GlobalSearchResult::FUNCTION, i, f, Classes[i].Functions[f].Name, Classes[i].Name + "::" + Classes[i].Functions[f].Name, Classes[i].Functions[f].ReturnType });
                }
            }
            for (int m = 0; m < Classes[i].Members.size(); m++) {
                if (StringContains(Classes[i].Members[m].Name, query)) {
                    GlobalResults.push_back({ GlobalSearchResult::PROPERTY, i, m, Classes[i].Members[m].Name, Classes[i].Name + "->" + Classes[i].Members[m].Name, Classes[i].Members[m].Type });
                }
            }
        }
        for (int i = 0; i < Enums.size(); i++) {
            if (StringContains(Enums[i].Name, query)) {
                GlobalResults.push_back({ GlobalSearchResult::ENUM, i, -1, Enums[i].Name, Enums[i].Name, Enums[i].Type });
            }
        }
        for (int i = 0; i < TypeDefs.size(); i++) {
            if (StringContains(TypeDefs[i].Alias, query)) {
                GlobalResults.push_back({ GlobalSearchResult::TYPEDEF, i, -1, TypeDefs[i].Alias, TypeDefs[i].Alias, TypeDefs[i].Underlying });
            }
        }
        std::sort(GlobalResults.begin(), GlobalResults.end(), [](const GlobalSearchResult& a, const GlobalSearchResult& b) { return a.Name.length() < b.Name.length(); });
    }

    // ---------------------------------------------------------
    // NEW: REVERSE CHAIN FINDER (BFS)
    // ---------------------------------------------------------
    void BuildReverseLinks() {
        ReverseReferenceMap.clear();
        for (int parentIdx = 0; parentIdx < Classes.size(); parentIdx++) {
            const auto& cls = Classes[parentIdx];
            for (const auto& member : cls.Members) {
                if (member.LinkedClassIdx != -1) {
                    ReverseReferenceMap[member.LinkedClassIdx].push_back({ parentIdx, member.Name, member.OffsetVal });
                }
            }
        }
    }

    std::vector<OffsetChainLink> FindChainToRoot(int targetClassIdx) {
        if (targetClassIdx < 0 || targetClassIdx >= Classes.size()) return {};

        // Definitions of "Root" classes
        std::vector<int> potentialRoots;
        if (ClassLookup.count("UWorld")) potentialRoots.push_back(ClassLookup["UWorld"]);
        if (ClassLookup.count("GWorld")) potentialRoots.push_back(ClassLookup["GWorld"]);
        if (ClassLookup.count("UEngine")) potentialRoots.push_back(ClassLookup["UEngine"]);
        if (ClassLookup.count("UGameInstance")) potentialRoots.push_back(ClassLookup["UGameInstance"]);

        // BFS Setup
        // Queue stores: CurrentClassIndex
        std::queue<int> q;
        q.push(targetClassIdx);

        // Track path: Key = ChildIdx, Value = Link Info (Who points to Child)
        struct PathNode {
            int ParentIdx;
            std::string MemberName;
            uint64_t Offset;
        };
        std::unordered_map<int, PathNode> pathTree;
        std::unordered_map<int, bool> visited;

        pathTree[targetClassIdx] = { -1, "", 0 };
        visited[targetClassIdx] = true;

        int foundRoot = -1;
        int safetyCounter = 0;

        while (!q.empty() && safetyCounter++ < 50000) {
            int current = q.front();
            q.pop();

            // Check if current is a root
            for (int r : potentialRoots) {
                if (current == r) {
                    foundRoot = r;
                    goto DoneSearching;
                }
            }

            // Expand parents (who points to current?)
            if (ReverseReferenceMap.count(current)) {
                for (const auto& link : ReverseReferenceMap[current]) {
                    if (!visited[link.ParentClassIdx]) {
                        visited[link.ParentClassIdx] = true;
                        pathTree[link.ParentClassIdx] = { current, link.MemberName, link.Offset }; // Store reversed for now
                        // Actually, pathTree should map Node -> Parent. 
                        // The link says "Parent points to Current".
                        // So in BFS (Target -> Root), the "Next Step" is Parent.
                        // We record: How did we get to Parent? Via Current.
                        // Wait, to reconstruct Root -> Target:
                        // We need Parent[Child]. 
                        // Here we are going Target(Child) -> Root(Parent).
                        // Let's store: cameFrom[Parent] = {Child, LinkInfo}? No.
                        // We want: parentMap[Child] = Parent.
                        // Currently traversing: Current (Child) -> Link.ParentClassIdx (Parent).

                        // Let's just store "Parent of Link.Parent is Current? No."
                        // We are walking up.
                        // pathTree[Parent] = { Child=Current, Offset=Link.Offset, Name=Link.Name }
                        // Then when we find Root, we look at pathTree[Root], get its child, etc.

                        // CORRECTION:
                        // pathTree Key: The Node we just found (The Parent)
                        // pathTree Value: The Node we came from (The Child/Current) + Reference Data
                        pathTree[link.ParentClassIdx] = { current, link.MemberName, link.Offset };
                        q.push(link.ParentClassIdx);
                    }
                }
            }
        }

    DoneSearching:
        if (foundRoot == -1) return {};

        // Reconstruct path: Root -> ... -> Target
        std::vector<OffsetChainLink> chain;
        int curr = foundRoot;
        uint64_t runningOffset = 0;

        // The chain should start with Root (implied) then offsets.
        // pathTree maps Parent -> Child.

        while (curr != targetClassIdx) {
            if (pathTree.find(curr) == pathTree.end()) break; // Should not happen
            PathNode& node = pathTree[curr];

            OffsetChainLink link;
            link.containerName = Classes[curr].Name;
            link.memberName = node.MemberName;
            link.offset = node.Offset;
            link.typeName = Classes[node.ParentIdx].Name; // The child type

            // Add to list
            chain.push_back(link);

            curr = node.ParentIdx; // Move to child
        }

        // Calculate totals
        for (auto& l : chain) {
            runningOffset += l.offset;
            l.totalOffset = runningOffset;
        }

        return chain;
    }

    void ParseDirectoryMultiThreaded(const std::string& DirectoryPath) {
        IsLoading = true;
        std::vector<std::string> filePaths;
        std::vector<std::string> fileNames;

        auto scanDir = [&](const std::string& dir) {
            if (!fs::exists(dir)) return;
            for (const auto& entry : fs::directory_iterator(dir)) {
                if (entry.is_regular_file()) {
                    std::string f = entry.path().filename().string();
                    if (f.find(".hpp") != std::string::npos || f.find(".h") != std::string::npos) {
                        filePaths.push_back(entry.path().string());
                        fileNames.push_back(f);
                    }
                }
            }
            };
        scanDir(DirectoryPath);
        scanDir(DirectoryPath + "\\SDK");

        TotalFiles = (int)filePaths.size();
        unsigned int numThreads = std::thread::hardware_concurrency();
        if (numThreads == 0) numThreads = 2;

        struct ThreadResult {
            std::vector<SDKClass> tClasses;
            std::vector<SDKEnum> tEnums;
            std::vector<SDKTypeDef> tTypeDefs;
        };
        std::vector<ThreadResult> threadResults(numThreads);
        std::vector<std::thread> threads;
        size_t filesPerThread = (size_t)TotalFiles / numThreads;

        LoadingStatus = "Parsing files...";
        for (unsigned int t = 0; t < numThreads; ++t) {
            size_t startIdx = t * filesPerThread;
            size_t endIdx = (t == numThreads - 1) ? (size_t)TotalFiles.load() : startIdx + filesPerThread;
            threads.emplace_back([this, startIdx, endIdx, &filePaths, &fileNames, &threadResults, t]() {
                for (size_t i = startIdx; i < endIdx; ++i) {
                    ParseFile(filePaths[i], fileNames[i], threadResults[t].tClasses, threadResults[t].tEnums, threadResults[t].tTypeDefs);
                    FilesProcessed++;
                }
                });
        }
        for (auto& th : threads) if (th.joinable()) th.join();

        LoadingStatus = "Merging results...";
        for (auto& res : threadResults) {
            Classes.insert(Classes.end(), std::make_move_iterator(res.tClasses.begin()), std::make_move_iterator(res.tClasses.end()));
            Enums.insert(Enums.end(), std::make_move_iterator(res.tEnums.begin()), std::make_move_iterator(res.tEnums.end()));
            TypeDefs.insert(TypeDefs.end(), std::make_move_iterator(res.tTypeDefs.begin()), std::make_move_iterator(res.tTypeDefs.end()));
        }

        LoadingStatus = "Sorting...";
        std::sort(Classes.begin(), Classes.end(), [](const SDKClass& a, const SDKClass& b) { return a.Name < b.Name; });
        std::sort(Enums.begin(), Enums.end(), [](const SDKEnum& a, const SDKEnum& b) { return a.Name < b.Name; });
        std::sort(TypeDefs.begin(), TypeDefs.end(), [](const SDKTypeDef& a, const SDKTypeDef& b) { return a.Alias < b.Alias; });

        LoadingStatus = "Linking types...";
        for (int i = 0; i < Classes.size(); i++) { ClassLookup[Classes[i].Name] = i; FilteredClassIndices.push_back(i); }
        for (int i = 0; i < Enums.size(); i++) { EnumLookup[Enums[i].Name] = i; FilteredEnumIndices.push_back(i); }
        for (int i = 0; i < TypeDefs.size(); i++) { TypeDefLookup[TypeDefs[i].Alias] = i; FilteredTypeDefIndices.push_back(i); }

        for (auto& cls : Classes) {
            if (!cls.ParentName.empty()) {
                std::string cleanParent = CleanType(cls.ParentName);
                if (ClassLookup.count(cleanParent)) {
                    cls.ParentClassIdx = ClassLookup[cleanParent];
                    DerivedLookup[cls.ParentClassIdx].push_back(ClassLookup[cls.Name]);
                }
            }
            for (auto& mem : cls.Members) {
                mem.CleanTypeName = CleanType(mem.Type);
                if (ClassLookup.count(mem.CleanTypeName)) mem.LinkedClassIdx = ClassLookup[mem.CleanTypeName];
                if (EnumLookup.count(mem.CleanTypeName)) mem.LinkedEnumIdx = EnumLookup[mem.CleanTypeName];
                if (TypeDefLookup.count(mem.CleanTypeName)) mem.LinkedTypeIdx = TypeDefLookup[mem.CleanTypeName];
            }
        }

        LoadingStatus = "Building Reverse Links...";
        BuildReverseLinks();

        IsLoading = false;
    }

    void ApplyFilters(const std::string& search) {
        FilteredClassIndices.clear();
        for (int i = 0; i < Classes.size(); i++) { if (search.empty() || StringContains(Classes[i].Name, search)) FilteredClassIndices.push_back(i); }
        FilteredEnumIndices.clear();
        for (int i = 0; i < Enums.size(); i++) { if (search.empty() || StringContains(Enums[i].Name, search)) FilteredEnumIndices.push_back(i); }
        FilteredTypeDefIndices.clear();
        for (int i = 0; i < TypeDefs.size(); i++) { if (search.empty() || StringContains(TypeDefs[i].Alias, search)) FilteredTypeDefIndices.push_back(i); }
    }

private:
    enum ParseState { NONE, IN_CLASS, IN_STRUCT, IN_ENUM };

    void ParseFile(const std::string& FilePath, const std::string& FileName,
        std::vector<SDKClass>& outClasses, std::vector<SDKEnum>& outEnums, std::vector<SDKTypeDef>& outTypeDefs)
    {
        std::ifstream file(FilePath);
        if (!file.is_open()) return;
        std::string line;
        ParseState state = NONE;
        SDKClass* currentClass = nullptr;
        SDKEnum* currentEnum = nullptr;

        while (std::getline(file, line)) {
            TrimString(line);
            if (line.empty() || line.find("//") == 0) continue;

            if (state == NONE) {
                if (line.find("typedef ") == 0) {
                    size_t lastSpace = line.find_last_of(' ');
                    size_t semicolon = line.find(';');
                    if (lastSpace != std::string::npos && semicolon != std::string::npos) {
                        outTypeDefs.push_back({ line.substr(lastSpace + 1, semicolon - lastSpace - 1), line.substr(8, lastSpace - 8), FileName });
                    }
                }
                else if (line.find("using ") == 0 && line.find(" = ") != std::string::npos) {
                    size_t eqPos = line.find(" = ");
                    size_t semicolon = line.find(';');
                    if (eqPos != std::string::npos && semicolon != std::string::npos) {
                        outTypeDefs.push_back({ line.substr(6, eqPos - 6), line.substr(eqPos + 3, semicolon - (eqPos + 3)), FileName });
                    }
                }
            }

            bool isClass = (line.find("class ") != std::string::npos);
            bool isStruct = (line.find("struct ") != std::string::npos);
            bool isEnum = (line.find("enum ") != std::string::npos);

            if (state == NONE && (isClass || isStruct) && !isEnum && line.find(";") == std::string::npos) {
                if (line.find(" : ") != std::string::npos || line.back() == '{' || line.find("alignas") != std::string::npos) {
                    SDKClass cls; cls.bIsStruct = isStruct; cls.SourceFile = FileName;
                    size_t nameStart = isStruct ? (line.find("struct ") + 7) : (line.find("class ") + 6);
                    size_t alignPos = line.find("alignas");
                    if (alignPos != std::string::npos && alignPos > nameStart) {
                        size_t closeParen = line.find(')', alignPos);
                        if (closeParen != std::string::npos) nameStart = closeParen + 1;
                    }
                    size_t nameEnd = line.find_first_of(":{}", nameStart);
                    if (nameEnd == std::string::npos) nameEnd = line.length();
                    cls.Name = line.substr(nameStart, nameEnd - nameStart);
                    TrimString(cls.Name);
                    size_t colon = line.find(" : public ");
                    if (colon != std::string::npos) {
                        cls.ParentName = line.substr(colon + 10);
                        size_t endP = cls.ParentName.find_first_of(" {");
                        if (endP != std::string::npos) cls.ParentName = cls.ParentName.substr(0, endP);
                        TrimString(cls.ParentName);
                    }
                    outClasses.push_back(cls); currentClass = &outClasses.back(); state = isStruct ? IN_STRUCT : IN_CLASS;
                }
            }
            else if (state == NONE && isEnum && line.back() == '{') {
                SDKEnum enm; enm.SourceFile = FileName;
                size_t nameStart = line.find("class ") != std::string::npos ? 11 : 5;
                size_t nameEnd = line.find_first_of(" :", nameStart);
                enm.Name = line.substr(nameStart, nameEnd - nameStart);
                TrimString(enm.Name);
                size_t typePos = line.find(" : ");
                if (typePos != std::string::npos) {
                    enm.Type = line.substr(typePos + 3); enm.Type.pop_back(); TrimString(enm.Type);
                }
                outEnums.push_back(enm); currentEnum = &outEnums.back(); state = IN_ENUM;
            }
            else if (state == IN_CLASS || state == IN_STRUCT) {
                if (line == "};") { state = NONE; currentClass = nullptr; continue; }
                size_t commentPos = line.find("// 0x");
                if (commentPos != std::string::npos) {
                    size_t semicolon = line.rfind(';', commentPos);
                    if (semicolon != std::string::npos) {
                        SDKMember member;
                        size_t offsetStart = commentPos + 5;
                        size_t offsetEnd = line.find('(', offsetStart);
                        if (offsetEnd != std::string::npos) {
                            member.OffsetStr = line.substr(offsetStart, offsetEnd - offsetStart);
                            member.OffsetVal = ParseHex(member.OffsetStr);
                            size_t sizeEnd = line.find(')', offsetEnd);
                            member.Size = line.substr(offsetEnd + 1, sizeEnd - (offsetEnd + 1));
                            size_t flagsStart = line.find('(', sizeEnd);
                            if (flagsStart != std::string::npos) {
                                size_t flagsEnd = line.find(')', flagsStart);
                                member.Flags = line.substr(flagsStart + 1, flagsEnd - (flagsStart + 1));
                            }
                        }
                        std::string def = line.substr(0, semicolon);
                        size_t nameSep = def.find_last_of(" *&>");
                        if (nameSep == std::string::npos) nameSep = def.find_last_of(' ');
                        if (nameSep != std::string::npos) {
                            member.Name = def.substr(nameSep + 1);
                            member.Type = def.substr(0, nameSep + 1);
                            TrimString(member.Type); TrimString(member.Name);
                            if (line.find("BitIndex") != std::string::npos) member.bIsBitfield = true;
                            currentClass->Members.push_back(member);
                        }
                    }
                }
                else if (line.find("(") != std::string::npos && line.find(")") != std::string::npos && line.back() == ';') {
                    if (line.find("static_assert") == std::string::npos && line.find("DECLARE_") == std::string::npos) {
                        SDKFunction func;
                        size_t openParen = line.find('(');
                        size_t nameStart = line.rfind(' ', openParen);
                        if (nameStart == std::string::npos) nameStart = line.rfind('*', openParen);
                        if (nameStart != std::string::npos) {
                            func.Name = line.substr(nameStart + 1, openParen - (nameStart + 1));
                            func.ReturnType = line.substr(0, nameStart + 1);
                            TrimString(func.ReturnType);
                            func.Params = line.substr(openParen); func.Params.pop_back();
                            currentClass->Functions.push_back(func);
                        }
                    }
                }
            }
            else if (state == IN_ENUM) {
                if (line == "};") { state = NONE; currentEnum = nullptr; continue; }
                size_t eqPos = line.find('=');
                if (eqPos != std::string::npos) {
                    SDKEnumMember mem;
                    mem.Name = line.substr(0, eqPos); TrimString(mem.Name);
                    size_t comma = line.find(',', eqPos);
                    if (comma == std::string::npos) comma = line.length();
                    mem.ValueStr = line.substr(eqPos + 1, comma - (eqPos + 1)); TrimString(mem.ValueStr);
                    try { mem.Value = std::stoll(mem.ValueStr); }
                    catch (...) {}
                    currentEnum->Members.push_back(mem);
                }
            }
        }
    }
};

// ---------------------------------------------------------
// MAIN UI & DIRECTX
// ---------------------------------------------------------

static ID3D11Device* g_pd3dDevice = nullptr;
static ID3D11DeviceContext* g_pd3dDeviceContext = nullptr;
static IDXGISwapChain* g_pSwapChain = nullptr;
static ID3D11RenderTargetView* g_mainRenderTargetView = nullptr;

bool CreateDeviceD3D(HWND hWnd);
void CleanupDeviceD3D();
void CreateRenderTarget();
void CleanupRenderTarget();
LRESULT WINAPI WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
    WNDCLASSEXW wc = { sizeof(wc), CS_CLASSDC, WndProc, 0L, 0L, GetModuleHandle(nullptr), nullptr, nullptr, nullptr, nullptr, L"SDKViewerDX11", nullptr };
    RegisterClassExW(&wc);
    HWND hwnd = CreateWindowW(wc.lpszClassName, L"SDK Viewer - Ultimate Edition", WS_OVERLAPPEDWINDOW, 100, 100, 1280, 800, nullptr, nullptr, wc.hInstance, nullptr);

    if (!CreateDeviceD3D(hwnd)) { CleanupDeviceD3D(); UnregisterClassW(wc.lpszClassName, wc.hInstance); return 1; }
    ShowWindow(hwnd, SW_SHOWDEFAULT); UpdateWindow(hwnd);

    IMGUI_CHECKVERSION(); ImGui::CreateContext(); ImGuiIO& io = ImGui::GetIO(); (void)io;
    ImGui::StyleColorsDark();

    ImGui_ImplWin32_Init(hwnd); ImGui_ImplDX11_Init(g_pd3dDevice, g_pd3dDeviceContext);

    SDKParser parser;
    NavHistory history;

    char searchBuffer[128] = "";
    char globalSearchBuf[128] = "";
    char innerSearchBuf[128] = "";
    char pathBuffer[256] = "C:\\Dumper-7\\SDK";

    int selectedClassIdx = -1;
    int selectedEnumIdx = -1;
    int selectedTypeDefIdx = -1;
    bool bShowInherited = true;

    int activeTab = 0;
    bool bRequestTabFocus = false;

    // Scroll handling (Delayed to next frame for clipper compatibility)
    bool bPendingScroll = false;
    int pendingScrollIdx = -1;

    std::string highlightName = "";
    int highlightType = 0;
    bool bOpenChainFinder = false;

    float leftPanelWidth = 350.0f;

    std::vector<OffsetChainLink> foundChain;
    std::string foundChainTarget = "";
    std::thread workerThread;

    bool done = false;
    while (!done) {
        MSG msg;
        while (::PeekMessage(&msg, nullptr, 0U, 0U, PM_REMOVE)) {
            ::TranslateMessage(&msg); ::DispatchMessage(&msg);
            if (msg.message == WM_QUIT) done = true;
        }
        if (done) break;

        ImGui_ImplDX11_NewFrame(); ImGui_ImplWin32_NewFrame(); ImGui::NewFrame();
        ImGui::SetNextWindowPos(ImVec2(0, 0)); ImGui::SetNextWindowSize(io.DisplaySize);
        ImGui::Begin("Main", nullptr, ImGuiWindowFlags_NoDecoration | ImGuiWindowFlags_NoResize | ImGuiWindowFlags_NoBringToFrontOnFocus);

        if (parser.IsLoading) ImGui::OpenPopup("LoadingData");
        if (ImGui::BeginPopupModal("LoadingData", NULL, ImGuiWindowFlags_AlwaysAutoResize | ImGuiWindowFlags_NoTitleBar)) {
            ImGui::Text("Parsing SDK... %s", parser.LoadingStatus.c_str());
            ImGui::ProgressBar(parser.TotalFiles > 0 ? (float)parser.FilesProcessed / (float)parser.TotalFiles : 0.0f, ImVec2(300, 0));
            if (!parser.IsLoading) { ImGui::CloseCurrentPopup(); if (workerThread.joinable()) workerThread.join(); }
            ImGui::EndPopup();
        }

        ImGui::BeginDisabled(parser.IsLoading);

        // --- HEADER ---
        if (ImGui::Button("Load SDK")) {
            parser.Clear(); selectedClassIdx = -1; selectedEnumIdx = -1; selectedTypeDefIdx = -1; history.Clear();
            if (workerThread.joinable()) workerThread.join();
            std::string p = pathBuffer; workerThread = std::thread([&parser, p]() { parser.ParseDirectoryMultiThreaded(p); });
        }
        ImGui::SameLine(); ImGui::InputText("Folder", pathBuffer, IM_ARRAYSIZE(pathBuffer));

        ImGui::SameLine();
        ImGui::Text("|"); ImGui::SameLine();
        if (parser.ClassLookup.count("UWorld")) {
            if (ImGui::Button("Go UWorld")) {
                selectedClassIdx = parser.ClassLookup["UWorld"];
                activeTab = 0; bRequestTabFocus = true;
                pendingScrollIdx = selectedClassIdx; bPendingScroll = true;
                memset(searchBuffer, 0, sizeof(searchBuffer)); parser.ApplyFilters("");
                history.Push(0, selectedClassIdx, -1, -1);
            }
            ImGui::SameLine();
        }
        else if (parser.ClassLookup.count("GWorld")) {
            if (ImGui::Button("Go GWorld")) {
                selectedClassIdx = parser.ClassLookup["GWorld"];
                activeTab = 0; bRequestTabFocus = true;
                pendingScrollIdx = selectedClassIdx; bPendingScroll = true;
                memset(searchBuffer, 0, sizeof(searchBuffer)); parser.ApplyFilters("");
                history.Push(0, selectedClassIdx, -1, -1);
            }
            ImGui::SameLine();
        }

        ImGui::Text(" | "); ImGui::SameLine();
        ImGui::SetNextItemWidth(300);
        if (ImGui::InputTextWithHint("##GlobalSearch", "Global Search...", globalSearchBuf, IM_ARRAYSIZE(globalSearchBuf), ImGuiInputTextFlags_EnterReturnsTrue)) {
            parser.PerformGlobalSearch(globalSearchBuf);
            activeTab = 2; bRequestTabFocus = true;
        }
        ImGui::SameLine();
        if (ImGui::Button("Search All")) {
            parser.PerformGlobalSearch(globalSearchBuf);
            activeTab = 2; bRequestTabFocus = true;
        }

        ImGui::Separator();

        // --- TABS ---
        if (ImGui::BeginTabBar("MainTabs")) {

            // 1. CLASSES TAB
            ImGuiTabItemFlags tab0Flags = (bRequestTabFocus && activeTab == 0) ? ImGuiTabItemFlags_SetSelected : 0;
            if (ImGui::BeginTabItem("Classes", nullptr, tab0Flags)) {
                activeTab = 0;

                ImGui::BeginChild("LeftC", ImVec2(leftPanelWidth, 0), true);
                if (ImGui::InputText("Filter", searchBuffer, IM_ARRAYSIZE(searchBuffer))) parser.ApplyFilters(searchBuffer);
                ImGui::Separator();

                ImGuiListClipper clipper;
                clipper.Begin((int)parser.FilteredClassIndices.size());
                while (clipper.Step()) {
                    for (int i = clipper.DisplayStart; i < clipper.DisplayEnd; i++) {
                        int realIdx = parser.FilteredClassIndices[i];
                        const auto& c = parser.Classes[realIdx];
                        ImVec4 col = c.bIsStruct ? ImVec4(0.6f, 0.8f, 1.0f, 1.0f) : ImVec4(1.0f, 1.0f, 1.0f, 1.0f);
                        ImGui::PushStyleColor(ImGuiCol_Text, col);

                        // Render selectable
                        if (ImGui::Selectable((c.Name + (c.bIsStruct ? " (Struct)" : "")).c_str(), selectedClassIdx == realIdx)) {
                            selectedClassIdx = realIdx;
                            highlightType = 0;
                            memset(innerSearchBuf, 0, sizeof(innerSearchBuf));
                            history.Push(0, realIdx, -1, -1);
                        }

                        // Handle scroll request for this item
                        if (bPendingScroll && pendingScrollIdx == realIdx) {
                            ImGui::SetScrollHereY();
                            bPendingScroll = false;
                            pendingScrollIdx = -1;
                        }

                        ImGui::PopStyleColor();
                    }
                }
                ImGui::EndChild();

                ImGui::SameLine();
                ImGui::InvisibleButton("vsplitter", ImVec2(8.0f, -1));
                if (ImGui::IsItemActive()) leftPanelWidth += ImGui::GetIO().MouseDelta.x;
                ImGui::SameLine();

                ImGui::BeginChild("RightC", ImVec2(0, 0), true);
                if (selectedClassIdx != -1 && selectedClassIdx < parser.Classes.size()) {
                    auto& cls = parser.Classes[selectedClassIdx];
                    if (ImGui::Button("< Back")) {
                        auto e = history.GoBack();
                        if (e.TabIndex != -1) {
                            activeTab = e.TabIndex; selectedClassIdx = e.ClassIdx;
                            selectedEnumIdx = e.EnumIdx; selectedTypeDefIdx = e.TypeDefIdx;
                            bRequestTabFocus = true;
                            pendingScrollIdx = selectedClassIdx; bPendingScroll = true;
                        }
                    }
                    ImGui::SameLine(); ImGui::TextColored(ImVec4(0.3f, 1.0f, 0.3f, 1.0f), "%s", cls.Name.c_str());
                    if (!cls.ParentName.empty()) { ImGui::SameLine(); ImGui::TextDisabled(": %s", cls.ParentName.c_str()); }

                    ImGui::Separator();
                    if (ImGui::Button("Find Pointer Chain to Here")) {
                        bOpenChainFinder = true;
                        foundChain.clear();
                        foundChainTarget = "";
                    }
                    ImGui::SameLine(); ImGui::Checkbox("Show Inherited", &bShowInherited);

                    // IN-CLASS SEARCH
                    ImGui::SameLine();
                    ImGui::SetNextItemWidth(200);
                    ImGui::InputTextWithHint("##InnerSearch", "Search Members...", innerSearchBuf, IM_ARRAYSIZE(innerSearchBuf));

                    if (bOpenChainFinder) { ImGui::OpenPopup("ChainFinder"); bOpenChainFinder = false; }
                    if (ImGui::BeginPopupModal("ChainFinder", NULL, ImGuiWindowFlags_AlwaysAutoResize)) {
                        ImGui::Text("Calculate pointer path: UWorld -> ... -> %s", cls.Name.c_str());
                        ImGui::TextDisabled("Note: Finds shortest path from UWorld/GWorld to this class.");

                        // Add optional property offset
                        static char propOffBuf[128] = "";
                        ImGui::InputText("Optional: Append Property (Name)", propOffBuf, 128);

                        if (ImGui::Button("Find Chain")) {
                            foundChain = parser.FindChainToRoot(selectedClassIdx);
                            foundChainTarget = propOffBuf;
                        }

                        ImGui::Separator();

                        if (!foundChain.empty()) {
                            std::stringstream stringChain;
                            if (parser.ClassLookup.count("GWorld")) stringChain << "GWorld";
                            else stringChain << "UWorld";

                            for (auto& link : foundChain) {
                                stringChain << " -> " << link.memberName;
                            }
                            if (!foundChainTarget.empty()) stringChain << " -> " << foundChainTarget;

                            ImGui::TextWrapped("%s", stringChain.str().c_str());
                            if (ImGui::Button("Copy Chain String")) ImGui::SetClipboardText(stringChain.str().c_str());

                            ImGui::Separator();

                            if (ImGui::BeginTable("ChainTbl", 3, ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg)) {
                                ImGui::TableSetupColumn("Container", ImGuiTableColumnFlags_WidthStretch);
                                ImGui::TableSetupColumn("Member", ImGuiTableColumnFlags_WidthStretch);
                                ImGui::TableSetupColumn("Offset", ImGuiTableColumnFlags_WidthFixed, 80);
                                ImGui::TableHeadersRow();
                                for (auto& link : foundChain) {
                                    ImGui::TableNextRow();
                                    ImGui::TableSetColumnIndex(0); ImGui::Text("%s", link.containerName.c_str());
                                    ImGui::TableSetColumnIndex(1); ImGui::Text("%s", link.memberName.c_str());
                                    ImGui::TableSetColumnIndex(2); ImGui::Text("0x%X", (int)link.offset);
                                }
                                ImGui::EndTable();
                            }
                        }
                        else {
                            if (foundChain.empty() && ImGui::IsItemHovered()) ImGui::SetTooltip("Click 'Find Chain' to start.");
                        }

                        if (ImGui::Button("Close")) { ImGui::CloseCurrentPopup(); propOffBuf[0] = 0; foundChain.clear(); }
                        ImGui::EndPopup();
                    }

                    if (ImGui::BeginTable("Mems", 4, ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg | ImGuiTableFlags_Resizable)) {
                        ImGui::TableSetupColumn("Offset", ImGuiTableColumnFlags_WidthFixed, 60);
                        ImGui::TableSetupColumn("Type", ImGuiTableColumnFlags_WidthFixed, 200);
                        ImGui::TableSetupColumn("Name", ImGuiTableColumnFlags_WidthStretch);
                        ImGui::TableSetupColumn("Size/Flags", ImGuiTableColumnFlags_WidthFixed, 150);
                        ImGui::TableHeadersRow();

                        std::vector<const SDKMember*> displayMems;
                        if (bShowInherited) {
                            SDKClass* ptr = &cls;
                            while (true) {
                                for (auto& m : ptr->Members) displayMems.push_back(&m);
                                if (ptr->ParentClassIdx == -1) break;
                                ptr = &parser.Classes[ptr->ParentClassIdx];
                            }
                            std::sort(displayMems.begin(), displayMems.end(), [](const SDKMember* a, const SDKMember* b) { return a->OffsetVal < b->OffsetVal; });
                        }
                        else {
                            for (auto& m : cls.Members) displayMems.push_back(&m);
                        }

                        for (int i = 0; i < displayMems.size(); ++i) {
                            auto* m = displayMems[i];

                            if (strlen(innerSearchBuf) > 0 && !StringContains(m->Name, innerSearchBuf)) continue;

                            bool isHighlighted = (highlightType == 1 && m->Name == highlightName);
                            if (isHighlighted) ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1, 1, 0, 1));

                            ImGui::TableNextRow();
                            ImGui::TableSetColumnIndex(0); ImGui::TextDisabled("0x%X", m->OffsetVal);
                            ImGui::TableSetColumnIndex(1);
                            if (m->LinkedClassIdx != -1) {
                                if (ImGui::Selectable(m->Type.c_str())) {
                                    selectedClassIdx = m->LinkedClassIdx;
                                    bRequestTabFocus = true;
                                    pendingScrollIdx = selectedClassIdx; bPendingScroll = true;
                                    memset(searchBuffer, 0, sizeof(searchBuffer)); parser.ApplyFilters("");
                                    history.Push(0, selectedClassIdx, -1, -1);
                                }
                            }
                            else if (m->LinkedTypeIdx != -1) {
                                if (ImGui::Selectable(m->Type.c_str())) { selectedTypeDefIdx = m->LinkedTypeIdx; activeTab = 1; bRequestTabFocus = true; history.Push(1, -1, -1, selectedTypeDefIdx); }
                            }
                            else { ImGui::Text("%s", m->Type.c_str()); }

                            ImGui::TableSetColumnIndex(2); ImGui::Text("%s", m->Name.c_str());
                            ImGui::TableSetColumnIndex(3); ImGui::TextDisabled("%s", m->Size.c_str());

                            if (isHighlighted) {
                                ImGui::PopStyleColor();
                                ImGui::SetScrollHereY();
                                highlightType = 0;
                            }
                        }
                        ImGui::EndTable();
                    }

                    if (highlightType == 2) ImGui::SetNextItemOpen(true);
                    if (ImGui::CollapsingHeader("Functions")) {
                        for (auto& f : cls.Functions) {
                            if (strlen(innerSearchBuf) > 0 && !StringContains(f.Name, innerSearchBuf)) continue;

                            bool isHighlighted = (highlightType == 2 && f.Name == highlightName);
                            if (isHighlighted) {
                                ImGui::PushStyleColor(ImGuiCol_Text, ImVec4(1, 1, 0, 1));
                                ImGui::SetScrollHereY();
                            }
                            ImGui::Bullet();
                            ImGui::TextColored(ImVec4(0.6f, 0.8f, 1, 1), "%s", f.ReturnType.c_str());
                            ImGui::SameLine();
                            ImGui::Text("%s%s", f.Name.c_str(), f.Params.c_str());
                            if (isHighlighted) {
                                ImGui::PopStyleColor();
                                highlightType = 0;
                            }
                        }
                    }
                }
                ImGui::EndChild();
                ImGui::EndTabItem();
            }

            // 2. TYPES & ENUMS TAB
            ImGuiTabItemFlags tab1Flags = (bRequestTabFocus && activeTab == 1) ? ImGuiTabItemFlags_SetSelected : 0;
            if (ImGui::BeginTabItem("Types", nullptr, tab1Flags)) {
                activeTab = 1;

                ImGui::BeginChild("LeftT", ImVec2(leftPanelWidth, 0), true);
                if (ImGui::CollapsingHeader("Enums", ImGuiTreeNodeFlags_DefaultOpen)) {
                    for (int idx : parser.FilteredEnumIndices) {
                        if (ImGui::Selectable(parser.Enums[idx].Name.c_str(), selectedEnumIdx == idx)) { selectedEnumIdx = idx; selectedTypeDefIdx = -1; }
                    }
                }
                if (ImGui::CollapsingHeader("TypeDefs", ImGuiTreeNodeFlags_DefaultOpen)) {
                    for (int idx : parser.FilteredTypeDefIndices) {
                        if (ImGui::Selectable(parser.TypeDefs[idx].Alias.c_str(), selectedTypeDefIdx == idx)) { selectedTypeDefIdx = idx; selectedEnumIdx = -1; }
                    }
                }
                ImGui::EndChild();

                ImGui::SameLine();
                ImGui::InvisibleButton("vsplitter2", ImVec2(8.0f, -1));
                if (ImGui::IsItemActive()) leftPanelWidth += ImGui::GetIO().MouseDelta.x;
                ImGui::SameLine();

                ImGui::BeginChild("RightT", ImVec2(0, 0), true);
                if (selectedTypeDefIdx != -1) {
                    ImGui::TextColored(ImVec4(0.5f, 1, 1, 1), "Typedef: %s", parser.TypeDefs[selectedTypeDefIdx].Alias.c_str());
                    ImGui::Text("Underlying: %s", parser.TypeDefs[selectedTypeDefIdx].Underlying.c_str());
                }
                else if (selectedEnumIdx != -1) {
                    auto& en = parser.Enums[selectedEnumIdx];
                    ImGui::TextColored(ImVec4(1, 0.8f, 0.5f, 1), "Enum: %s", en.Name.c_str());
                    ImGui::BeginTable("EnumVals", 2, ImGuiTableFlags_Borders);
                    for (auto& m : en.Members) {
                        ImGui::TableNextRow();
                        ImGui::TableSetColumnIndex(0); ImGui::Text("%s", m.Name.c_str());
                        ImGui::TableSetColumnIndex(1); ImGui::Text("%lld", m.Value);
                    }
                    ImGui::EndTable();
                }
                ImGui::EndChild();
                ImGui::EndTabItem();
            }

            // 3. GLOBAL SEARCH RESULTS TAB
            ImGuiTabItemFlags tab2Flags = (bRequestTabFocus && activeTab == 2) ? ImGuiTabItemFlags_SetSelected : 0;
            if (ImGui::BeginTabItem("Search Results", nullptr, tab2Flags)) {
                activeTab = 2;
                ImGui::Text("Results for: '%s' (%d found)", globalSearchBuf, (int)parser.GlobalResults.size());
                ImGui::Separator();

                if (ImGui::BeginTable("GRes", 4, ImGuiTableFlags_Borders | ImGuiTableFlags_RowBg | ImGuiTableFlags_ScrollY)) {
                    ImGui::TableSetupColumn("Action", ImGuiTableColumnFlags_WidthFixed, 60);
                    ImGui::TableSetupColumn("Type", ImGuiTableColumnFlags_WidthFixed, 80);
                    ImGui::TableSetupColumn("Name / Context", ImGuiTableColumnFlags_WidthStretch);
                    ImGui::TableSetupColumn("Info", ImGuiTableColumnFlags_WidthFixed, 150);
                    ImGui::TableHeadersRow();

                    ImGuiListClipper sClip;
                    sClip.Begin((int)parser.GlobalResults.size());
                    while (sClip.Step()) {
                        for (int i = sClip.DisplayStart; i < sClip.DisplayEnd; i++) {
                            auto& res = parser.GlobalResults[i];
                            ImGui::PushID(i);
                            ImGui::TableNextRow();

                            // COL 0: Action Button
                            ImGui::TableSetColumnIndex(0);
                            if (ImGui::SmallButton("Go")) {
                                if (res.Type == GlobalSearchResult::CLASS || res.Type == GlobalSearchResult::STRUCT) {
                                    activeTab = 0; selectedClassIdx = res.ContainerIdx; highlightType = 0;
                                    bRequestTabFocus = true;
                                    pendingScrollIdx = selectedClassIdx; bPendingScroll = true;
                                    memset(searchBuffer, 0, sizeof(searchBuffer));
                                    parser.ApplyFilters("");
                                    history.Push(0, selectedClassIdx, -1, -1);
                                }
                                else if (res.Type == GlobalSearchResult::PROPERTY || res.Type == GlobalSearchResult::FUNCTION) {
                                    activeTab = 0; selectedClassIdx = res.ContainerIdx;
                                    highlightName = res.Name;
                                    highlightType = (res.Type == GlobalSearchResult::PROPERTY) ? 1 : 2;
                                    bRequestTabFocus = true;
                                    pendingScrollIdx = selectedClassIdx; bPendingScroll = true;
                                    memset(searchBuffer, 0, sizeof(searchBuffer));
                                    parser.ApplyFilters("");
                                    history.Push(0, selectedClassIdx, -1, -1);
                                }
                                else if (res.Type == GlobalSearchResult::ENUM) {
                                    activeTab = 1; selectedEnumIdx = res.ContainerIdx; selectedTypeDefIdx = -1;
                                    bRequestTabFocus = true; history.Push(1, -1, selectedEnumIdx, -1);
                                }
                                else if (res.Type == GlobalSearchResult::TYPEDEF) {
                                    activeTab = 1; selectedTypeDefIdx = res.ContainerIdx; selectedEnumIdx = -1;
                                    bRequestTabFocus = true; history.Push(1, -1, -1, selectedTypeDefIdx);
                                }
                            }

                            // COL 1: Type Label
                            ImGui::TableSetColumnIndex(1);
                            const char* label = "???";
                            ImVec4 color = ImVec4(1, 1, 1, 1);
                            switch (res.Type) {
                            case GlobalSearchResult::CLASS: label = "CLASS"; color = ImVec4(0.4f, 1, 0.4f, 1); break;
                            case GlobalSearchResult::STRUCT: label = "STRUCT"; color = ImVec4(0.4f, 0.8f, 0.4f, 1); break;
                            case GlobalSearchResult::FUNCTION: label = "FUNC"; color = ImVec4(0.4f, 0.8f, 1, 1); break;
                            case GlobalSearchResult::PROPERTY: label = "PROP"; color = ImVec4(1, 0.8f, 0.4f, 1); break;
                            case GlobalSearchResult::ENUM: label = "ENUM"; color = ImVec4(1, 0.4f, 1, 1); break;
                            case GlobalSearchResult::TYPEDEF: label = "TYPE"; color = ImVec4(0.8f, 0.8f, 0.8f, 1); break;
                            }
                            ImGui::TextColored(color, "%s", label);

                            // COL 2: Name & Context Menu
                            ImGui::TableSetColumnIndex(2);
                            ImGui::Selectable(res.Context.c_str(), false, ImGuiSelectableFlags_SpanAllColumns);
                            if (ImGui::BeginPopupContextItem("Ctx")) {
                                if (res.Type == GlobalSearchResult::CLASS || res.Type == GlobalSearchResult::STRUCT ||
                                    res.Type == GlobalSearchResult::FUNCTION || res.Type == GlobalSearchResult::PROPERTY) {
                                    if (ImGui::Selectable("Go to Class")) {
                                        activeTab = 0; selectedClassIdx = res.ContainerIdx; highlightType = 0;
                                        bRequestTabFocus = true;
                                        pendingScrollIdx = selectedClassIdx; bPendingScroll = true;
                                        memset(searchBuffer, 0, sizeof(searchBuffer));
                                        parser.ApplyFilters("");
                                        history.Push(0, selectedClassIdx, -1, -1);
                                    }
                                    if (ImGui::Selectable("Copy Class C++")) {
                                        ImGui::SetClipboardText(parser.GenerateClassDump(res.ContainerIdx).c_str());
                                    }
                                }
                                if (res.Type == GlobalSearchResult::PROPERTY) {
                                    if (ImGui::Selectable("Find Chain to Here")) {
                                        activeTab = 0; selectedClassIdx = res.ContainerIdx;
                                        bRequestTabFocus = true;
                                        pendingScrollIdx = selectedClassIdx; bPendingScroll = true;
                                        memset(searchBuffer, 0, sizeof(searchBuffer));
                                        parser.ApplyFilters("");
                                        bOpenChainFinder = true;
                                    }
                                }
                                if (ImGui::Selectable("Copy Name")) ImGui::SetClipboardText(res.Name.c_str());
                                ImGui::EndPopup();
                            }

                            ImGui::TableSetColumnIndex(3);
                            ImGui::TextDisabled("%s", res.ExtraInfo.c_str());

                            ImGui::PopID();
                        }
                    }
                    ImGui::EndTable();
                }
                ImGui::EndTabItem();
            }

            ImGui::EndTabBar();
        }

        bRequestTabFocus = false;

        ImGui::EndDisabled();
        ImGui::End();

        ImGui::Render();
        const float clear_color_with_alpha[4] = { 0.1f, 0.1f, 0.13f, 1.0f };
        g_pd3dDeviceContext->OMSetRenderTargets(1, &g_mainRenderTargetView, nullptr);
        g_pd3dDeviceContext->ClearRenderTargetView(g_mainRenderTargetView, clear_color_with_alpha);
        ImGui_ImplDX11_RenderDrawData(ImGui::GetDrawData());
        g_pSwapChain->Present(1, 0);
    }

    if (workerThread.joinable()) workerThread.join();
    ImGui_ImplDX11_Shutdown(); ImGui_ImplWin32_Shutdown(); ImGui::DestroyContext();
    CleanupDeviceD3D(); DestroyWindow(hwnd); UnregisterClassW(wc.lpszClassName, wc.hInstance);
    return 0;
}

// ---------------------------------------------------------
// DIRECTX SETUP
// ---------------------------------------------------------
bool CreateDeviceD3D(HWND hWnd) {
    DXGI_SWAP_CHAIN_DESC sd; ZeroMemory(&sd, sizeof(sd));
    sd.BufferCount = 2; sd.BufferDesc.Width = 0; sd.BufferDesc.Height = 0;
    sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
    sd.BufferDesc.RefreshRate.Numerator = 60; sd.BufferDesc.RefreshRate.Denominator = 1;
    sd.Flags = DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH;
    sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT; sd.OutputWindow = hWnd;
    sd.SampleDesc.Count = 1; sd.SampleDesc.Quality = 0; sd.Windowed = TRUE;
    sd.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;
    UINT createDeviceFlags = 0;
    D3D_FEATURE_LEVEL featureLevel;
    const D3D_FEATURE_LEVEL featureLevelArray[2] = { D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_0, };
    if (D3D11CreateDeviceAndSwapChain(nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr, createDeviceFlags, featureLevelArray, 2, D3D11_SDK_VERSION, &sd, &g_pSwapChain, &g_pd3dDevice, &featureLevel, &g_pd3dDeviceContext) != S_OK)
        return false;
    CreateRenderTarget();
    return true;
}

void CleanupDeviceD3D() {
    CleanupRenderTarget();
    if (g_pSwapChain) { g_pSwapChain->Release(); g_pSwapChain = nullptr; }
    if (g_pd3dDeviceContext) { g_pd3dDeviceContext->Release(); g_pd3dDeviceContext = nullptr; }
    if (g_pd3dDevice) { g_pd3dDevice->Release(); g_pd3dDevice = nullptr; }
}

void CreateRenderTarget() {
    ID3D11Texture2D* pBackBuffer;
    g_pSwapChain->GetBuffer(0, IID_PPV_ARGS(&pBackBuffer));
    g_pd3dDevice->CreateRenderTargetView(pBackBuffer, nullptr, &g_mainRenderTargetView);
    pBackBuffer->Release();
}

void CleanupRenderTarget() {
    if (g_mainRenderTargetView) { g_mainRenderTargetView->Release(); g_mainRenderTargetView = nullptr; }
}

extern IMGUI_IMPL_API LRESULT ImGui_ImplWin32_WndProcHandler(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
LRESULT WINAPI WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam) {
    if (ImGui_ImplWin32_WndProcHandler(hWnd, msg, wParam, lParam)) return true;
    switch (msg) {
    case WM_SIZE:
        if (g_pd3dDevice != nullptr && wParam != SIZE_MINIMIZED) {
            CleanupRenderTarget();
            g_pSwapChain->ResizeBuffers(0, (UINT)LOWORD(lParam), (UINT)HIWORD(lParam), DXGI_FORMAT_UNKNOWN, 0);
            CreateRenderTarget();
        }
        return 0;
    case WM_SYSCOMMAND:
        if ((wParam & 0xfff0) == SC_KEYMENU) return 0;
        break;
    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;
    }
    return DefWindowProc(hWnd, msg, wParam, lParam);
}
