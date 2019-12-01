import Foundation

public func ingestFile(_ filename: String) -> [String.SubSequence] {
    do {
        return try String(contentsOfFile: filename)
            .split(separator: "\n")
    } catch {
        preconditionFailure("Couldn't load file '\(filename)': \(error)")
    }
}

/// Run a parser over the lines of the given file, producing a list of parsed objects
/// - Parameters:
///   - parser: Parser of lines in the file to some object
///   - filename: Name of the file to parse
/// - Returns: list of parsed objects
public func parseFile<A>(_ parser: Parser<A, String>, _ filename: String) -> [A] {
    ingestFile(filename)
        .enumerated()
        .map { lineNumber, originalInput -> A in
            var input = originalInput[...]
            guard let match = parser.run(&input) else {
                preconditionFailure("Couldn't parse line \(lineNumber), which is: \n\(originalInput)")
            }
            guard input.count == 0 else {
                preconditionFailure("""
                Didn't completely parse line \(lineNumber), which is:
                \(originalInput)

                Remaining unparsed text, in quotes:
                '\(input)'
                """)
            }
            return match
        }
}

public func force<A>(_ a: A?) -> A { a! }
