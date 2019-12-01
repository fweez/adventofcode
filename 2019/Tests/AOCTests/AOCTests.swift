import XCTest
import class Foundation.Bundle

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
