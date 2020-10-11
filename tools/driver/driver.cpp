//------------------------------------------------------------------------------
// driver.cpp
// Entry point for the primary driver program.
//
// File is under the MIT license; see LICENSE for details
//------------------------------------------------------------------------------

#include "slang/compilation/Compilation.h"
#include "slang/compilation/Definition.h"
#include "slang/diagnostics/DiagnosticEngine.h"
#include "slang/diagnostics/TextDiagnosticClient.h"
#include "slang/numeric/ConstantValue.h"
#include "slang/parsing/Preprocessor.h"
#include "slang/symbols/ASTSerializer.h"
#include "slang/symbols/CompilationUnitSymbols.h"
#include "slang/symbols/InstanceSymbols.h"
#include "slang/symbols/ParameterSymbols.h"
#include "slang/symbols/Type.h"
#include "slang/syntax/SyntaxPrinter.h"
#include "slang/syntax/SyntaxTree.h"
#include "slang/text/Json.h"
#include "slang/text/SourceManager.h"
#include "slang/util/CommandLine.h"
#include "slang/util/OS.h"
#include "slang/util/String.h"
#include "slang/util/Version.h"

#include <cassert>
#include <fstream>
#include <iostream>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#if defined(INCLUDE_SIM)
#    include "slang/codegen/JIT.h"
#    include "slang/mir/MIRBuilder.h"
#endif

using namespace slang;

static constexpr auto noteColor = fmt::terminal_color::bright_black;
static constexpr auto warningColor = fmt::terminal_color::bright_yellow;
static constexpr auto errorColor = fmt::terminal_color::bright_red;
static constexpr auto highlightColor = fmt::terminal_color::bright_green;

void writeToFile(string_view fileName, string_view contents);

bool runPreprocessor(SourceManager& sourceManager, const Bag& options,
                     const std::vector<SourceBuffer>& buffers, bool includeComments,
                     bool includeDirectives) {
    BumpAllocator alloc;
    Diagnostics diagnostics;
    Preprocessor preprocessor(sourceManager, alloc, diagnostics, options);

    for (auto it = buffers.rbegin(); it != buffers.rend(); it++)
        preprocessor.pushSource(*it);

    SyntaxPrinter output;
    output.setIncludeComments(includeComments);
    output.setIncludeDirectives(includeDirectives);

    while (true) {
        Token token = preprocessor.next();
        output.print(token);
        if (token.kind == TokenKind::EndOfFile)
            break;
    }

    // Only print diagnostics if actual errors occurred.
    for (auto& diag : diagnostics) {
        if (diag.isError()) {
            OS::print("{}", DiagnosticEngine::reportAll(sourceManager, diagnostics));
            return false;
        }
    }

    OS::print("{}\n", output.str());
    return true;
}

void printMacros(SourceManager& sourceManager, const Bag& options,
                 const std::vector<SourceBuffer>& buffers) {
    BumpAllocator alloc;
    Diagnostics diagnostics;
    Preprocessor preprocessor(sourceManager, alloc, diagnostics, options);

    for (auto it = buffers.rbegin(); it != buffers.rend(); it++)
        preprocessor.pushSource(*it);

    while (true) {
        Token token = preprocessor.next();
        if (token.kind == TokenKind::EndOfFile)
            break;
    }

    for (auto macro : preprocessor.getDefinedMacros()) {
        SyntaxPrinter printer;
        printer.setIncludeComments(false);
        printer.setIncludeTrivia(false);
        printer.print(macro->name);

        printer.setIncludeTrivia(true);
        if (macro->formalArguments)
            printer.print(*macro->formalArguments);
        printer.print(macro->body);

        OS::print("{}\n", printer.str());
    }
}

static bool ensureFile(std::ofstream& fout, const std::string& path) {
    fout.open(path);
    return fout.good();
}

static bool doDumpMacros(Compilation& compilation, const std::string& path) {
    std::ofstream fout;
    if (!ensureFile(fout, path)) {
        return false;
    }
    auto indent = [&]() {
        for (int i = 0; i < 4; ++i) {
            fout << ' ';
        }
    };
    auto syntaxTrees = compilation.getSyntaxTrees();
    for (const auto & syntaxTree: syntaxTrees) {
        for (const auto & pair: syntaxTree->modulesMacros) {
            auto * modDecl = pair.first;
            const auto& vec = pair.second;
            fout << modDecl->header->name.rawText() << '\n';
            for (const auto* def: vec) {
                indent();
                // TODO: round trip to source
                fout << def->name.rawText();
                // maybe dump formal arguments
                if (def->formalArguments) {
                    fout << '(';
                    bool first = true;
                    for (auto argTok: def->formalArguments->args) {
                        if (!first) {
                            fout << ", ";
                        }
                        first = false;
                        fout << argTok->name.rawText();
                    }
                    fout << ')';
                }
                fout << " : ";
                // dump body
                for (Token tok: def->body) {
                    fout << tok.rawText();
                    fout << ' ';
                }
                fout << '\n';
            }
        }
    }
    return true;
}
template <typename T>
void dumpConstantValue(std::ofstream& fout, const T& val) {
    fout << val;
}

void dumpConstantValue(std::ofstream&, const std::monostate&) {
     assert(false && "impossible to have std::monostate as ConstantValue");
}

class RecursiveInstanceParameterDumper {
    Compilation &compilation;
    std::ofstream &fout;
    std::vector<std::string_view> hierName;
    int indentLevel = 4;
    char indentChar = ' ';
    void indent(int level = 1) {
        level *= indentLevel;
        while (level--) {
            fout << indentChar;
        }
    }
    void dumpHierarchicalName() {
        bool first = true;
        for (auto name: hierName) {
            if (!first) {
                fout << '.';
            }
            first = false;
            fout << name;
        }
    }
    bool maybeDump(const InstanceSymbol& instance) {
        std::vector<const ParameterSymbol*> value_parameters;
        std::vector<const TypeParameterSymbol*> type_parameters;
        const auto& body = instance.body;
        for (const auto& sym: body.members()) {
            switch (sym.kind) {
                default: {
                    continue;
                }
                case SymbolKind::Parameter: {
                    value_parameters.push_back(static_cast<const ParameterSymbol*>(&sym));
                    break;
                }
                case SymbolKind::TypeParameter: {
                    type_parameters.push_back(static_cast<const TypeParameterSymbol*>(&sym));
                    break;
                }
            }
        }
        bool shouldDump = !value_parameters.empty() || !type_parameters.empty();
        if (shouldDump) {
            dumpHierarchicalName();
            fout << '\n';
        }
        if (!type_parameters.empty()) {
            indent();
            fout << "type parameters:\n";
            for (const auto* param: type_parameters) {
                indent(2);
                fout << param->name << " : " << param->targetType.getType().name << '\n';
            }
            fout << '\n';
        }
        if (!value_parameters.empty()) {
            indent();
            fout << "value parameters:\n";
            for (const auto* param: value_parameters) {
                indent(2);
                fout << param->name << " : ";
                std::visit([this](auto&& arg){dumpConstantValue(this->fout, arg);}, param->getValue().getVariant());
                fout << '\n';
            }
        }
        if (shouldDump) {
            fout << '\n';
        }
        return true;
    }
    bool visitMembers(slang::iterator_range<Scope::iterator> members_range) {
        for (const auto& sym: members_range) {
            switch (sym.kind) {
                default: {
                    continue;
                }
                case SymbolKind::Instance: {
                    const auto& insSym = static_cast<const InstanceSymbol &>(sym);
                    if (!visit(insSym)) {
                        return false;
                    }
                    break;
                }
                case SymbolKind::InstanceArray: {
                    const auto& insSym = static_cast<const InstanceArraySymbol &>(sym);
                    if (!visit(insSym)) {
                        return false;
                    }
                    break;
                }
            }
        }
        return true;
    }
public:
    RecursiveInstanceParameterDumper(Compilation& compilation, std::ofstream& fout):
        compilation(compilation), fout(fout) {
    }
    bool visit(const InstanceSymbol& instance) {
        hierName.push_back(instance.name);
        maybeDump(instance);
        const auto& body = instance.body;
        visitMembers(body.members());
        hierName.pop_back();
        return true;
    }
    bool visit(const InstanceArraySymbol& instance) {
        int32_t range = instance.range.right, range_end = instance.range.left;
        // TODO: make sure the order matches.
        int32_t range_delta = range < range_end ? 1 : -1;
        const Symbol * const * ele = instance.elements.data();
        const Symbol * const * ele_end = ele + instance.elements.size();
        for (; range < range_end; range += range_delta, ele++) {
                std::string ins_name = std::string(instance.name) + "[" + std::to_string(range) + "]";
                hierName.push_back(ins_name);
                switch((*ele)->kind) {
                    default: {
                        std::cerr << "unknown array instance kind: " << (*ele)->kind << '\n';
                        assert(false);
                        break;
                    }
                    case SymbolKind::Instance: {
                        const auto& ins = static_cast<const InstanceSymbol&>(**ele);
                        maybeDump(ins);
                        const auto& body = ins.body;
                        visitMembers(body.members());
                        break;
                    }
                }
                hierName.pop_back();
        }
        return true;
    }
};

static bool doDumpParameters(Compilation& compilation, const std::string& path) {
    std::ofstream fout;
    if (!ensureFile(fout, path)) {
        return false;
    }
    RecursiveInstanceParameterDumper dumper(compilation, fout);
    auto topInstances = compilation.getRoot().topInstances;
    for (auto inst : topInstances) {
        if (!dumper.visit(*inst)) {
            return false;
        }
    }
    return true;
}

bool runCompiler(Compilation& compilation, const std::vector<std::string>& warningOptions,
                 uint32_t errorLimit, bool quiet, bool onlyParse, bool showColors,
                 const optional<std::string>& astJsonFile, const std::optional<std::string>& dumpModuleMacros) {
    DiagnosticEngine diagEngine(*compilation.getSourceManager());
    Diagnostics optionDiags = diagEngine.setWarningOptions(warningOptions);
    Diagnostics pragmaDiags = diagEngine.setMappingsFromPragmas();
    diagEngine.setErrorLimit(errorLimit);

    auto client = std::make_shared<TextDiagnosticClient>();
    client->setColorsEnabled(showColors);
    diagEngine.addClient(client);

    for (auto& diag : optionDiags)
        diagEngine.issue(diag);

    for (auto& diag : pragmaDiags)
        diagEngine.issue(diag);

    if (onlyParse) {
        for (auto& diag : compilation.getParseDiagnostics())
            diagEngine.issue(diag);
    }
    else {
#ifndef FUZZ_TARGET
        auto topInstances = compilation.getRoot().topInstances;
        if (!quiet && !topInstances.empty()) {
            OS::print(fg(warningColor), "Top level design units:\n");
            for (auto inst : topInstances)
                OS::print("    {}\n", inst->name);
            OS::print("\n");
        }
#endif

        for (auto& diag : compilation.getAllDiagnostics())
            diagEngine.issue(diag);
    }

    if (dumpModuleMacros && !doDumpMacros(compilation, dumpModuleMacros.value())) {
        return false;
    }

    if (const auto & maybeDumpParameters = compilation.getOptions().dumpParameters;
        maybeDumpParameters && !doDumpParameters(compilation, maybeDumpParameters.value())) {
        return false;
    }

    if (astJsonFile) {
        JsonWriter writer;
        writer.setPrettyPrint(true);

        ASTSerializer serializer(compilation, writer);
        serializer.serialize(compilation.getRoot());

        writeToFile(*astJsonFile, writer.view());
    }

    bool succeeded = diagEngine.getNumErrors() == 0;

#ifndef FUZZ_TARGET
    std::string diagStr = client->getString();
    OS::print("{}", diagStr);

    if (!quiet && !onlyParse) {
        if (diagStr.size() > 1)
            OS::print("\n");

        if (succeeded)
            OS::print(fg(highlightColor), "Build succeeded: ");
        else
            OS::print(fg(errorColor), "Build failed: ");

        OS::print("{} error{}, {} warning{}\n", diagEngine.getNumErrors(),
                  diagEngine.getNumErrors() == 1 ? "" : "s", diagEngine.getNumWarnings(),
                  diagEngine.getNumWarnings() == 1 ? "" : "s");
    }
#endif

    return succeeded;
}

#if defined(INCLUDE_SIM)
using namespace slang::mir;

bool runSim(Compilation& compilation) {
    MIRBuilder builder(compilation);
    builder.elaborate();

    CodeGenerator codegen(compilation);
    codegen.emitAll(builder);

    JIT jit;
    jit.addCode(codegen.finish());
    return jit.run() == 0;
}
#endif

template<typename TArgs>
int driverMain(int argc, TArgs argv, bool suppressColors) try {
    CommandLine cmdLine;

    // General
    optional<bool> showHelp;
    optional<bool> showVersion;
    optional<bool> quiet;
    cmdLine.add("-h,--help", showHelp, "Display available options");
    cmdLine.add("-v,--version", showVersion, "Display version information and exit");
    cmdLine.add("-q,--quiet", quiet, "Suppress non-essential output");

    // Output control
    optional<bool> onlyPreprocess;
    optional<bool> onlyParse;
    optional<bool> onlyMacros;
    cmdLine.add("-E,--preprocess", onlyPreprocess,
                "Only run the preprocessor (and print preprocessed files to stdout)");
    cmdLine.add("--macros-only", onlyMacros, "Print a list of found macros and exit");
    cmdLine.add("--parse-only", onlyParse,
                "Stop after parsing input files, don't perform elaboration or type checking");

    // Include paths
    std::vector<std::string> includeDirs;
    std::vector<std::string> includeSystemDirs;
    cmdLine.add("-I,--include-directory", includeDirs, "Additional include search paths", "<dir>");
    cmdLine.add("--isystem", includeSystemDirs, "Additional system include search paths", "<dir>");

    // Preprocessor
    optional<bool> includeComments;
    optional<bool> includeDirectives;
    optional<uint32_t> maxIncludeDepth;
    std::vector<std::string> defines;
    std::vector<std::string> undefines;
    std::optional<std::string> dumpParameters;
    std::optional<std::string> dumpModuleMacros;
    cmdLine.add("-D,--define-macro", defines,
                "Define <macro> to <value> (or 1 if <value> ommitted) in all source files",
                "<macro>=<value>");
    cmdLine.add("-U,--undefine-macro", undefines,
                "Undefine macro name at the start of all source files", "<macro>");
    cmdLine.add("--comments", includeComments, "Include comments in preprocessed output (with -E)");
    cmdLine.add("--directives", includeDirectives,
                "Include compiler directives in preprocessed output (with -E)");
    cmdLine.add("--max-include-depth", maxIncludeDepth,
                "Maximum depth of nested include files allowed", "<depth>");

    // Parsing
    optional<uint32_t> maxParseDepth;
    optional<uint32_t> maxLexerErrors;
    cmdLine.add("--max-parse-depth", maxParseDepth,
                "Maximum depth of nested language constructs allowed", "<depth>");
    cmdLine.add("--max-lexer-errors", maxLexerErrors,
                "Maximum number of errors that can occur during lexing before the rest of the file "
                "is skipped",
                "<count>");

    // JSON dumping
    optional<std::string> astJsonFile;
    cmdLine.add("--ast-json", astJsonFile,
                "Dump the compiled AST in JSON format to the specified file, or '-' for stdout",
                "<file>");

    // Compilation
    optional<uint32_t> maxInstanceDepth;
    optional<uint32_t> maxGenerateSteps;
    optional<uint32_t> maxConstexprDepth;
    optional<uint32_t> maxConstexprSteps;
    optional<uint32_t> maxConstexprBacktrace;
    optional<std::string> minTypMax;
    std::vector<std::string> topModules;
    cmdLine.add("--max-hierarchy-depth", maxInstanceDepth, "Maximum depth of the design hierarchy",
                "<depth>");
    cmdLine.add("--max-generate-steps", maxGenerateSteps,
                "Maximum number of steps that can occur during generate block "
                "evaluation before giving up",
                "<steps>");
    cmdLine.add("--max-constexpr-depth", maxConstexprDepth,
                "Maximum depth of a constant evaluation call stack", "<depth>");
    cmdLine.add("--max-constexpr-steps", maxConstexprSteps,
                "Maximum number of steps that can occur during constant "
                "evaluation before giving up",
                "<steps>");
    cmdLine.add("--constexpr-backtrace-limit", maxConstexprBacktrace,
                "Maximum number of frames to show when printing a constant evaluation "
                "backtrace; the rest will be abbreviated",
                "<limit>");
    cmdLine.add("-T,--timing", minTypMax,
                "Select which value to consider in min:typ:max expressions", "min|typ|max");
    cmdLine.add("--top", topModules,
                "One or more top-level modules to instantiate "
                "(instead of figuring it out automatically)",
                "<name>");

    // Diagnostics control
    optional<bool> colorDiags;
    optional<uint32_t> errorLimit;
    std::vector<std::string> warningOptions;
    cmdLine.add("-W", warningOptions, "Control the specified warning", "<warning>");
    cmdLine.add("--color-diagnostics", colorDiags,
                "Always print diagnostics in color."
                "If this option is unset, colors will be enabled if a color-capable "
                "terminal is detected.");
    cmdLine.add("--error-limit", errorLimit,
                "Limit on the number of errors that will be printed. Setting this to zero will "
                "disable the limit.",
                "<limit>");

    // File list
    optional<bool> singleUnit;
    std::vector<std::string> sourceFiles;
    cmdLine.add("--single-unit", singleUnit, "Treat all input files as a single compilation unit");
    cmdLine.setPositional(sourceFiles, "files");

#if defined(INCLUDE_SIM)
    // Simulation
    optional<bool> shouldSim;
    cmdLine.add("--sim", shouldSim, "After compiling, try to simulate the design");
#endif

    cmdLine.add("--dump-macros", dumpModuleMacros, "Dump all the macros that are defined when modules are defined");
    cmdLine.add("--dump-parameters", dumpParameters, "Dump all instantiated module instance's parameters");

    if (!cmdLine.parse(argc, argv)) {
        for (auto& err : cmdLine.getErrors())
            OS::print("{}\n", err);
        return 1;
    }

    if (showHelp == true) {
        OS::print("{}", cmdLine.getHelpText("slang SystemVerilog compiler"));
        return 0;
    }

    if (showVersion == true) {
        OS::print("slang version {}.{}.{}\n", VersionInfo::getMajor(), VersionInfo::getMinor(),
                  VersionInfo::getRevision());
        return 0;
    }

    bool showColors;
    if (colorDiags)
        showColors = *colorDiags;
    else
        showColors = !suppressColors && OS::fileSupportsColors(stdout);

    if (showColors)
        OS::setColorsEnabled(true);

    bool anyErrors = false;
    SourceManager sourceManager;
    for (const std::string& dir : includeDirs) {
        try {
            sourceManager.addUserDirectory(string_view(dir));
        }
        catch (const std::exception&) {
            OS::print(fg(errorColor), "error: ");
            OS::print("include directory '{}' does not exist\n", dir);
            anyErrors = true;
        }
    }

    for (const std::string& dir : includeSystemDirs) {
        try {
            sourceManager.addSystemDirectory(string_view(dir));
        }
        catch (const std::exception&) {
            OS::print(fg(errorColor), "error: ");
            OS::print("include directory '{}' does not exist\n", dir);
            anyErrors = true;
        }
    }

    PreprocessorOptions ppoptions;
    ppoptions.predefines = defines;
    ppoptions.undefines = undefines;
    ppoptions.predefineSource = "<command-line>";
    if (maxIncludeDepth.has_value())
        ppoptions.maxIncludeDepth = *maxIncludeDepth;

    LexerOptions loptions;
    if (maxLexerErrors.has_value())
        loptions.maxErrors = *maxLexerErrors;

    ParserOptions poptions;
    if (maxParseDepth.has_value())
        poptions.maxRecursionDepth = *maxParseDepth;
    if (dumpModuleMacros) {
        poptions.dumpModuleMacros = true;
    }

    CompilationOptions coptions;
    coptions.suppressUnused = false;
    if (maxInstanceDepth.has_value())
        coptions.maxInstanceDepth = *maxInstanceDepth;
    if (maxGenerateSteps.has_value())
        coptions.maxGenerateSteps = *maxGenerateSteps;
    if (maxConstexprDepth.has_value())
        coptions.maxConstexprDepth = *maxConstexprDepth;
    if (maxConstexprSteps.has_value())
        coptions.maxConstexprSteps = *maxConstexprSteps;
    if (maxConstexprBacktrace.has_value())
        coptions.maxConstexprBacktrace = *maxConstexprBacktrace;
    if (errorLimit.has_value())
        coptions.errorLimit = *errorLimit * 2;

    for (auto& name : topModules)
        coptions.topModules.emplace(name);

    if (minTypMax.has_value()) {
        if (minTypMax == "min")
            coptions.minTypMax = MinTypMax::Min;
        else if (minTypMax == "typ")
            coptions.minTypMax = MinTypMax::Typ;
        else if (minTypMax == "max")
            coptions.minTypMax = MinTypMax::Max;
        else {
            OS::print(fg(errorColor), "error: ");
            OS::print("invalid value for timing option: '{}'", *minTypMax);
            return 1;
        }
    }
    coptions.dumpParameters = dumpParameters;

    Bag options;
    options.set(ppoptions);
    options.set(loptions);
    options.set(poptions);
    options.set(coptions);

    std::vector<SourceBuffer> buffers;
    for (const std::string& file : sourceFiles) {
        SourceBuffer buffer = sourceManager.readSource(file);
        if (!buffer) {
            OS::print(fg(errorColor), "error: ");
            OS::print("no such file or directory: '{}'\n", file);
            anyErrors = true;
            continue;
        }

        buffers.push_back(buffer);
    }

    if (anyErrors)
        return 2;

    if (buffers.empty()) {
        OS::print(fg(errorColor), "error: ");
        OS::print("no input files\n");
        return 3;
    }

    if (onlyParse.has_value() + onlyPreprocess.has_value() + onlyMacros.has_value() > 1) {
        OS::print(fg(errorColor), "error: ");
        OS::print("can only specify one of --preprocess, --macros-only, --parse-only");
        return 4;
    }

    try {
        if (onlyPreprocess == true) {
            anyErrors = !runPreprocessor(sourceManager, options, buffers, includeComments == true,
                                         includeDirectives == true);
        }
        else if (onlyMacros == true) {
            printMacros(sourceManager, options, buffers);
        }
        else {
            Compilation compilation(options);
            if (singleUnit == true) {
                compilation.addSyntaxTree(SyntaxTree::fromBuffers(buffers, sourceManager, options));
            }
            else {
                for (const SourceBuffer& buffer : buffers)
                    compilation.addSyntaxTree(
                        SyntaxTree::fromBuffer(buffer, sourceManager, options));
            }

            anyErrors = !runCompiler(compilation, warningOptions, errorLimit.value_or(20),
                                     quiet == true, onlyParse == true, showColors, astJsonFile, dumpModuleMacros);

#if defined(INCLUDE_SIM)
            if (!anyErrors && !onlyParse.value_or(false) && shouldSim == true) {
                anyErrors = !runSim(compilation);
            }
#endif
        }
    }
    catch (const std::exception& e) {
#ifdef FUZZ_TARGET
        (void)e;
        throw;
#else
        OS::print("internal compiler error: {}\n", e.what());
        return 4;
#endif
    }

    return anyErrors ? 1 : 0;
}
catch (const std::exception& e) {
#ifdef FUZZ_TARGET
    (void)e;
    throw;
#else
    OS::print("{}\n", e.what());
    return 5;
#endif
}

template<typename Stream, typename String>
void writeToFile(Stream& os, string_view fileName, String contents) {
    os.write(contents.data(), contents.size());
    os.flush();
    if (!os)
        throw std::runtime_error(fmt::format("Unable to write AST to '{}'", fileName));
}

#if defined(_MSC_VER)
#    include <Windows.h>
#    include <fcntl.h>
#    include <io.h>

void writeToFile(string_view fileName, string_view contents) {
    if (fileName == "-") {
        writeToFile(std::wcout, "stdout", widen(contents));
    }
    else {
        std::ofstream file(widen(fileName));
        writeToFile(file, fileName, contents);
    }
}

#    ifndef FUZZ_TARGET
int wmain(int argc, wchar_t** argv) {
    _setmode(_fileno(stdout), _O_U16TEXT);

    // Try to enable ANSI-style color handling.
    bool suppressColors = true;
    HANDLE hOut = GetStdHandle(STD_OUTPUT_HANDLE);
    if (hOut != INVALID_HANDLE_VALUE) {
        DWORD mode = 0;
        if (GetConsoleMode(hOut, &mode)) {
            mode |= ENABLE_VIRTUAL_TERMINAL_PROCESSING;
            if (SetConsoleMode(hOut, mode))
                suppressColors = false;
        }
    }

    return driverMain(argc, argv, suppressColors);
}
#    endif

#else

void writeToFile(string_view fileName, string_view contents) {
    if (fileName == "-") {
        writeToFile(std::cout, "stdout", contents);
    }
    else {
        std::ofstream file{ std::string(fileName) };
        writeToFile(file, fileName, contents);
    }
}

#    ifndef FUZZ_TARGET
int main(int argc, char** argv) {
    return driverMain(argc, argv, false);
}
#    endif

#endif

// When fuzzing with libFuzzer, this is the entry point.
#ifdef FUZZ_TARGET
extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
    string_view text(reinterpret_cast<const char*>(data), size);

    Compilation compilation;
    compilation.addSyntaxTree(SyntaxTree::fromText(text, "<source>"));

    runCompiler(compilation, {}, 0, /* quiet */ false, /* onlyParse */ false,
                /* showColors */ false, {}, {});

    return 0;
}
#endif
