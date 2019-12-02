import XCTest
import class Foundation.Bundle

@testable import AOCShared

/// To start a day, copy and paste this line, replacing the day
@testable import day1

/// To start a day, copy and paste this test class
final class DayXTests: XCTestCase {
    
}

/// To start a day, copy and paste this line, replacing the day
@testable import day1

/// To start a day, copy and paste this test class
final class Day1Tests: XCTestCase {
    func testPart1() {
        XCTAssertEqual(calculateModuleFuel(12), 2)
        XCTAssertEqual(calculateModuleFuel(14), 2)
        XCTAssertEqual(calculateModuleFuel(1969), 654)
        XCTAssertEqual(calculateModuleFuel(100756), 33583)
    }
    
    func testPart2() {
        XCTAssertEqual(calculateTotalFuel(12), 2)
        XCTAssertEqual(calculateTotalFuel(14), 2)
        XCTAssertEqual(calculateTotalFuel(1969), 966)
        XCTAssertEqual(calculateTotalFuel(100756), 50346)
    }
}

/// To start a day, copy and paste this line, replacing the day
@testable import day2

/// To start a day, copy and paste this test class
final class Day2Tests: XCTestCase {
    func testOpcodeProcessor() {
        XCTAssertEqual(runIntcodeProgram([1,9,10,3,2,3,11,0,99,30,40,50]), [3500,9,10,70,2,3,11,0,99,30,40,50])
        XCTAssertEqual(runIntcodeProgram([1,0,0,0,99]), [2,0,0,0,99])
        XCTAssertEqual(runIntcodeProgram([2,3,0,3,99]), [2,3,0,6,99])
        XCTAssertEqual(runIntcodeProgram([2,4,4,5,99,0]), [2,4,4,5,99,9801])
        XCTAssertEqual(runIntcodeProgram([1,1,1,4,99,5,6,0,99]), [30,1,1,4,2,5,6,0,99])
    }
}
