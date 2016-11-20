//------------------------------------------------------------------------------
// Diagnostics.cpp
// Diagnostic tracking and reporting.
//
// File is under the MIT license; see LICENSE for details.
//------------------------------------------------------------------------------
#include "Diagnostics.h"

#include <algorithm>

#include "../external/fmt/format.h"
#include "../external/fmt/ostream.h"

#include "SourceManager.h"

namespace slang {

static const char* severityToString[] = {
    "note",
    "warning",
    "error"
};

Diagnostic::Diagnostic(DiagCode code, SourceLocation location) :
    code(code), location(location)
{
}

Diagnostic& operator<<(Diagnostic& diag, Diagnostic::Arg&& arg) {
    if (arg.target<SourceRange>())
        diag.ranges.push_back(get<SourceRange>(arg));
    else
        diag.args.push_back(std::move(arg));
    return diag;
}

std::ostream& operator<<(std::ostream& os, const Diagnostic::Arg& arg) {
    return apply([&](auto&& t) -> auto& { return os << t; }, arg);
}

Diagnostics::Diagnostics() :
    Buffer::Buffer(8)
{
}

Diagnostic& Diagnostics::add(DiagCode code, SourceLocation location) {
    emplace(code, location);
    return back();
}

DiagnosticWriter::DiagnosticWriter(SourceManager& sourceManager) :
	sourceManager(sourceManager)
{
	// lexer
	descriptors[DiagCode::NonPrintableChar] = { "non-printable character in source text; SystemVerilog only supports ASCII text", DiagnosticSeverity::Error };
	descriptors[DiagCode::UTF8Char] = { "UTF-8 sequence in source text; SystemVerilog only supports ASCII text", DiagnosticSeverity::Error };
	descriptors[DiagCode::UnicodeBOM] = { "Unicode BOM at start of source text; SystemVerilog only supports ASCII text", DiagnosticSeverity::Error };
	descriptors[DiagCode::EmbeddedNull] = { "embedded NUL in source text; are you sure this is source code?", DiagnosticSeverity::Error };
	descriptors[DiagCode::MisplacedDirectiveChar] = { "expected directive name", DiagnosticSeverity::Error };
	descriptors[DiagCode::EscapedWhitespace] = { "unexpected whitespace after escape character", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedClosingQuote] = { "missing closing quote", DiagnosticSeverity::Error };
	descriptors[DiagCode::UnterminatedBlockComment] = { "block comment unclosed at end of file", DiagnosticSeverity::Error };
	descriptors[DiagCode::NestedBlockComment] = { "nested block comments are disallowed by SystemVerilog", DiagnosticSeverity::Error };
	descriptors[DiagCode::SplitBlockCommentInDirective] = { "block comments on the same line as a directive must also be terminated on that line", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedIntegerBaseAfterSigned] = { "expected integer base specifier after signed specifier", DiagnosticSeverity::Error };
	descriptors[DiagCode::MissingFractionalDigits] = { "expected fractional digits after decimal", DiagnosticSeverity::Error };
	descriptors[DiagCode::OctalEscapeCodeTooBig] = { "octal escape code is too large to be an ASCII character", DiagnosticSeverity::Error };
	descriptors[DiagCode::InvalidHexEscapeCode] = { "invalid hexadecimal number", DiagnosticSeverity::Error };
	descriptors[DiagCode::UnknownEscapeCode] = { "unknown character escape sequence", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedIncludeFileName] = { "expected an include file name", DiagnosticSeverity::Error };
	descriptors[DiagCode::MissingExponentDigits] = { "expected exponent digits", DiagnosticSeverity::Error };
	descriptors[DiagCode::VectorDigitsLeadingUnderscore] = { "vector literals must not start with a leading underscore", DiagnosticSeverity::Error };
	descriptors[DiagCode::DecimalDigitMultipleUnknown] = { "decimal literals cannot have multiple X or Z digits", DiagnosticSeverity::Error };
	descriptors[DiagCode::BadBinaryDigit] = { "expected binary digit", DiagnosticSeverity::Error };
	descriptors[DiagCode::BadOctalDigit] = { "expected octal digit", DiagnosticSeverity::Error };
	descriptors[DiagCode::BadDecimalDigit] = { "expected decimal digit", DiagnosticSeverity::Error };
	descriptors[DiagCode::BadHexDigit] = { "expected hexadecimal digit", DiagnosticSeverity::Error };

	// numeric
	descriptors[DiagCode::LiteralSizeIsZero] = { "size of vector literal cannot be zero", DiagnosticSeverity::Error };
	descriptors[DiagCode::LiteralSizeTooLarge] = { "size of vector literal is too large (> {} bits)", DiagnosticSeverity::Error };
	descriptors[DiagCode::RealExponentOverflow] = { "real literal overflows 64 bits", DiagnosticSeverity::Error };
	descriptors[DiagCode::SignedIntegerOverflow] = { "signed integer overflows 32 bits", DiagnosticSeverity::Error };
	descriptors[DiagCode::DecimalLiteralOverflow] = { "decimal literal overflows 32 bits", DiagnosticSeverity::Error };
	descriptors[DiagCode::VectorLiteralOverflow] = { "vector literal too large for the given number of bits", DiagnosticSeverity::Error };

	// preprocessor
	descriptors[DiagCode::CouldNotOpenIncludeFile] = { "could not find or open include file", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExceededMaxIncludeDepth] = { "exceeded max include depth", DiagnosticSeverity::Error };
	descriptors[DiagCode::UnknownDirective] = { "unknown macro or compiler directive", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedEndOfDirective] = { "expected end of directive (missing newline?)", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedEndOfMacroArgs] = { "expected end of macro arguments (missing closing parenthesis?)", DiagnosticSeverity::Error };
	descriptors[DiagCode::UnexpectedConditionalDirective] = { "unexpected conditional directive", DiagnosticSeverity::Error };
	descriptors[DiagCode::UnbalancedMacroArgDims] = { "unbalanced macro argument delimiters ((), [], or {{}}); didn't see an end '{}'", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedMacroArgs] = { "expected macro arguments for function-like macro", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedNetType] = { "expected net type specifier", DiagnosticSeverity::Error };
	descriptors[DiagCode::InvalidMacroName] = { "can't redefine compiler directive as a macro", DiagnosticSeverity::Error };
	descriptors[DiagCode::TooManyActualMacroArgs] = { "too many arguments provided to function-like macro", DiagnosticSeverity::Error };
	descriptors[DiagCode::NotEnoughMacroArgs] = { "not enough arguments provided to function-like macro", DiagnosticSeverity::Error };

	// parser
	descriptors[DiagCode::ExpectedIdentifier] = { "expected identifier", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedToken] = { "expected '{}'", DiagnosticSeverity::Error };
	descriptors[DiagCode::ImplicitNotAllowed] = { "expected data type (implicit type name not allowed)", DiagnosticSeverity::Error };
	descriptors[DiagCode::MultipleTypesInDeclaration] = { "multiple types given in single declaration; this is not allowed in SystemVerilog", DiagnosticSeverity::Error };
	descriptors[DiagCode::ColonShouldBeDot] = { "misplaced colon; did you mean to use a dot?", DiagnosticSeverity::Error };
	descriptors[DiagCode::InvalidTokenInMemberList] = { "invalid token in member list", DiagnosticSeverity::Error };
	descriptors[DiagCode::InvalidTokenInSequentialBlock] = { "invalid token in sequential block", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedStatement] = { "expected statement", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedParameterPort] = { "expected parameter declaration", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedNonAnsiPort] = { "expected non-ansi port declaration", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedAnsiPort] = { "expected ansi port declaration", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedForInitializer] = { "expected for loop initializer", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedExpression] = { "expected expression", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedOpenRangeElement] = { "expected open range element", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedStreamExpression] = { "expected stream expression", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedArgument] = { "expected argument", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedVariableDeclarator] = { "expected variable declarator", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedConditionalPattern] = { "expected conditional pattern", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedAttribute] = { "expected attribute", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedPackageImport] = { "expected package import", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedHierarchicalInstantiation] = { "expected hierarhical instantiation", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedPortConnection] = { "expected port connection", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedVectorDigits] = { "expected vector literal digits", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedVariableAssignment] = { "expected variable assignment", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedInterfaceClassName] = { "expected interface class name", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedAssignmentKey] = { "expected assignment key", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedDistItem] = { "expected dist item", DiagnosticSeverity::Error };
	descriptors[DiagCode::ExpectedIfOrCase] = { "expected 'if' or 'case' after '{}' keyword", DiagnosticSeverity::Error };
	descriptors[DiagCode::NoLabelOnSemicolon] = { "labels are not allowed on empty semicolon", DiagnosticSeverity::Error };
	descriptors[DiagCode::DeferredDelayMustBeZero] = { "deferred assertion delay must be zero", DiagnosticSeverity::Error };
	descriptors[DiagCode::AttributesNotSupported] = { "attributes are not allowed to be attached to {}", DiagnosticSeverity::Error };

	// declarations
	descriptors[DiagCode::DuplicateModule] = { "duplicate module definition '{}'", DiagnosticSeverity::Error };
	descriptors[DiagCode::NotePreviousDefinition] = { "previous definition here", DiagnosticSeverity::Note };
	descriptors[DiagCode::UnknownModule] = { "unknown module named '{}'", DiagnosticSeverity::Error };
	descriptors[DiagCode::DuplicateParameter] = { "duplicate parameter definition '{}'", DiagnosticSeverity::Error };
	descriptors[DiagCode::LocalParamNoInitializer] = { "local parameter is missing an initializer", DiagnosticSeverity::Error };
	descriptors[DiagCode::BodyParamNoInitializer] = { "parameters declaration is missing an initializer", DiagnosticSeverity::Error };
	descriptors[DiagCode::PackedDimRequiresConstantRange] = { "packed dimension requires a constant range", DiagnosticSeverity::Error };
	descriptors[DiagCode::PackedDimsOnPredefinedType] = { "packed dimensions not allowed on predefined integer type '{}'", DiagnosticSeverity::Error };

	ASSERT((int)DiagCode::MaxValue == descriptors.size(), "When you add a new diagnostic code you need to update default messages");
}

void DiagnosticWriter::setMessage(DiagCode code, std::string format) {
	descriptors[code].format = std::move(format);
}

void DiagnosticWriter::setSeverity(DiagCode code, DiagnosticSeverity severity) {
	descriptors[code].severity = severity;
}

DiagnosticSeverity DiagnosticWriter::getSeverity(DiagCode code) const {
	return descriptors.find(code)->second.severity;
}

std::string DiagnosticWriter::report(const Diagnostic& diagnostic) {
	// walk out until we find a location for this diagnostic that isn't inside a macro
	Buffer<SourceLocation> expansionLocs;
	SourceLocation location = diagnostic.location;
	while (sourceManager.isMacroLoc(location)) {
		expansionLocs.append(location);
		location = sourceManager.getExpansionLoc(location);
	}

	// build the error message from arguments, if we have any
	Descriptor& desc = descriptors[diagnostic.code];
	std::string msg;
	switch (diagnostic.args.size()) {
		case 0: msg = desc.format; break;
		case 1: msg = fmt::format(desc.format, diagnostic.args[0]); break;
		case 2: msg = fmt::format(desc.format, diagnostic.args[0], diagnostic.args[1]); break;
		default:
			ASSERT(false, "Too many arguments to diagnostic format. Add another switch case!");
	}

	fmt::MemoryWriter writer;
	formatDiag(writer, location, diagnostic.ranges, severityToString[(int)desc.severity], msg);

	// write out macro expansions, if we have any
	while (!expansionLocs.empty()) {
		location = expansionLocs.back();
		expansionLocs.pop();
		formatDiag(writer, sourceManager.getOriginalLoc(location), std::vector<SourceRange>(),
                   "note", "expanded from here");
	}

	return writer.str();
}

std::string DiagnosticWriter::report(Diagnostics& diagnostics) {
    // first sort diagnostics by file so that we can cut down
    // on the amount of include information we print out
    std::sort(diagnostics.begin(), diagnostics.end(), [this](auto& x, auto& y) { return sortDiagnostics(x, y); });

    std::deque<SourceLocation> includeStack;
    BufferID lastBuffer;
    fmt::MemoryWriter writer;

    for (auto& diag : diagnostics) {
        SourceLocation loc = getFullyExpandedLoc(diag.location);
        if (loc.buffer() != lastBuffer) {
            // We're looking at diagnostics from another file now. See if we should print
            // include stack info before we go on with the reports.
            lastBuffer = loc.buffer();
            getIncludeStack(lastBuffer, includeStack);

            for (auto& includeLoc : includeStack) {
                writer.write("In file included from {}:{}:\n",
                    sourceManager.getBufferName(includeLoc.buffer()),
                    sourceManager.getLineNumber(includeLoc)
                );
            }
        }
		writer << report(diag);
    }
    return writer.str();
}

SourceLocation DiagnosticWriter::getFullyExpandedLoc(SourceLocation loc) {
	while (sourceManager.isMacroLoc(loc))
		loc = sourceManager.getExpansionLoc(loc);
	return loc;
}

bool DiagnosticWriter::sortDiagnostics(const Diagnostic& x, const Diagnostic& y) {
	// start by expanding out macro locations
	SourceLocation xl = getFullyExpandedLoc(x.location);
	SourceLocation yl = getFullyExpandedLoc(y.location);
	return xl < yl;
}

StringRef DiagnosticWriter::getBufferLine(SourceLocation location, uint32_t col) {
	StringRef text = sourceManager.getSourceText(location.buffer());
	if (!text)
		return nullptr;

	const char* start = text.begin() + location.offset() - (col - 1);
	const char* curr = start;
	while (*curr != '\n' && *curr != '\r' && *curr != '\0')
		curr++;

	return StringRef(start, (uint32_t)(curr - start));
}

void DiagnosticWriter::getIncludeStack(BufferID buffer, std::deque<SourceLocation>& stack) {
	stack.clear();
	while (buffer) {
		SourceLocation loc = sourceManager.getIncludedFrom(buffer);
		if (!loc.buffer())
			break;

		stack.push_front(loc);
		buffer = loc.buffer();
	}
}

void DiagnosticWriter::highlightRange(SourceRange range, SourceLocation caretLoc, uint32_t col,
                                      StringRef sourceLine, std::string& buffer) {
    // If the end location is within a macro, we want to push it out to the
    // end of the expanded location so that it encompasses the entire macro usage
    SourceLocation startLoc = getFullyExpandedLoc(range.start());
    SourceLocation endLoc = range.end();
    while (sourceManager.isMacroLoc(endLoc)) {
        SourceRange endRange = sourceManager.getExpansionRange(endLoc);
        if (!sourceManager.isMacroLoc(endRange.start()))
            endLoc = endRange.end();
        else
            endLoc = endRange.start();
    }

    // If they're in different files just give up
    if (startLoc.buffer() != caretLoc.buffer() || endLoc.buffer() != caretLoc.buffer())
        return;

    // Trim the range so that it only falls on the same line as the cursor
    uint32_t start = startLoc.offset();
    uint32_t end = endLoc.offset();
    uint32_t startOfLine = caretLoc.offset() - (col - 1);
    uint32_t endOfLine = startOfLine + sourceLine.length();
    if (start < startOfLine)
        start = startOfLine;
    if (end > endOfLine)
        end = endOfLine;

    if (start >= end)
        return;

    // walk the range in to skip any leading or trailing whitespace
    start -= startOfLine;
    end -= startOfLine;
    while (sourceLine[start] == ' ' || sourceLine[start] == '\t') {
        start++;
        if (start == end)
            return;
    }
    while (sourceLine[end - 1] == ' ' || sourceLine[end - 1] == '\t') {
        end--;
        if (start == end)
            return;
    }

    // finally add the highlight chars
    for (; start != end; start++)
        buffer[start] = '~';
}

template<typename T>
void DiagnosticWriter::formatDiag(T& writer, SourceLocation loc, const std::vector<SourceRange>& ranges,
                                  const char* severity, const std::string& msg) {
	uint32_t col = sourceManager.getColumnNumber(loc);
	writer.write("{}:{}:{}: {}: {}",
		sourceManager.getBufferName(loc.buffer()),
		sourceManager.getLineNumber(loc),
		col,
		severity,
		msg
	);

	StringRef line = getBufferLine(loc, col);
	if (line) {
		writer.write("\n{}\n", line);

        // Highlight any ranges and print the caret location.
        // TODO: handle tabs
        std::string buffer(line.length(), ' ');
        for (SourceRange range : ranges)
            highlightRange(range, loc, col, line, buffer);

        buffer[col - 1] = '^';
        writer << buffer;
	}
	writer << '\n';
}

}