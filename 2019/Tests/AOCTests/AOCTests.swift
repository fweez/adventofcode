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


/// To start a day, copy and paste this line, replacing the day
@testable import day3

/// To start a day, copy and paste this test class
final class Day3Tests: XCTestCase {
    func testIntersections() {
        let wires1 = textToWires(["R8,U5,L5,D3", "U7,R6,D4,L4"]).map(generateSegments)
        let intersections1 = getAllIntersections(wires1.first!, wires1.last!)
        XCTAssertEqual(intersections1.count, 2)
        XCTAssertEqual(intersections1.first!.x, 6)
        XCTAssertEqual(intersections1.first!.y, 5)
        XCTAssertEqual(intersections1.last!.x, 3)
        XCTAssertEqual(intersections1.last!.y, 3)
    }
    
    func testDistances() {
        XCTAssertEqual(distToClosestIntersection(["R8,U5,L5,D3", "U7,R6,D4,L4"]), 6)
        XCTAssertEqual(distToClosestIntersection(["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"]), 159)
        XCTAssertEqual(distToClosestIntersection(["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]), 135)
    }
    
    func testGuesses() {
        let filelines: [String.SubSequence] = [
            "R999,D586,L462,D725,L236,U938,R366,D306,R263,D355,R354,D332,L599,U48,R829,U210,R697,D534,L19,U991,L110,U981,L954,U323,R851,U290,R76,D513,R844,D780,L257,D24,L586,U865,L341,U572,L122,D304,R398,D641,L221,U726,R270,D321,R503,D112,L151,D179,R439,U594,R242,U1,L484,D259,L604,U760,R362,D93,R29,D647,R482,U814,L214,D510,R281,U327,L170,D993,R191,D33,L305,D657,L897,U609,R512,D866,R654,U980,L899,D602,L141,D365,L13,D584,L706,U404,L238,U720,L732,U716,R672,U979,L49,D352,R712,U396,L843,D816,L276,U906,L375,D410,R275,U664,R487,D158,L713,D451,L859,U194,L736,D51,R659,U632,R586,U342,L222,U184,R741,U989,L175,D521,R820,U183,L950,D888,R54,D149,R776,D200,R939,U529,L377,D226,R769,U395,R392,U570,L398,D358,L644,D975,R578,D687,L133,D884,R822,D226,L527,U439,R175,D388,L539,D450,L391,U392,L131,U134,R873,U741,R761,U620,R667,D31,R481,D945,L373,D463,R57,D402,R181,U340,L835,U81,R908,U257,R592,U702,R713,D352,R418,D486,L904,U866,R828,D545,R578,U469,L845,D437,R371,D246,L996,D920,L171,U83,R471,D152,R550,U344,L390,U287,L126,D883,L576,U303,L68,U854,L463,D915,R184,D282,L513,U909,R770,U638,L751,U168,R354,D480,R19,U144,R381,D554,R594,D526,L957,D464,R267,D802,L709,U306,L907,D266,L871,U286,R975,D549,L732,U721,R825,U753,R443,U465,L966,U982,L833,D62,L5,U299,R500,D168,R155,D102,R455,D855,L376,D479,L469,D6,R588,U301,R329,U19,L63,D488,L936,D238,L798,D452,L231,D652,R935,D522,L401,U234,L20,U285,L949,D88,L120,D159,R641,D960,L946,U516,L530,D447,R23,U962,R860,D352,R904,D241,R702,U108,L155,U99,L43,D401,R19",
            "L1008,U23,L793,D944,L109,U830,L103,U255,L391,D574,R433,U468,R800,D831,L39,U8,L410,D467,R655,D287,R550,U467,L627,D529,R361,D865,L755,D895,L148,U110,R593,U567,L646,D89,L133,D552,R576,U228,L119,U734,R591,U680,L163,D498,L394,U884,R217,U46,R684,D499,L522,U373,L322,U347,R48,D459,L692,U569,R267,U296,L949,U915,R599,D113,R770,U322,R304,U920,L880,D257,R915,D672,L950,U209,R601,U663,R461,D514,R415,U82,L396,U233,R606,U500,R70,D696,R945,D686,L405,U176,R728,U562,L710,D35,R707,D931,L857,U792,R337,D490,L963,U731,R909,U532,R375,D990,L154,U660,L17,U32,R593,U529,R136,U835,R717,U255,L93,D295,L473,U608,L109,D858,R719,U207,R60,D36,R790,D382,L684,D233,R988,U625,R410,U804,R552,D578,L440,D749,R653,U362,L900,U549,R790,D870,R672,U503,R343,D343,R738,D270,R494,D527,L182,U654,R933,D594,R447,U933,R4,U364,L309,U967,R648,U537,R990,U203,R584,D474,L852,U736,R305,D781,R774,D92,L398,U207,R472,D664,R369,U807,L474,U588,R339,D536,R305,D506,R516,U772,R177,U450,L211,U850,R777,U483,L595,U104,L916,U548,R256,U173,L27,D167,L574,D288,R569,U192,R771,D98,R432,U165,L651,D524,L582,D698,L393,D152,L280,U461,R573,D771,R833,D409,R991,U996,R780,U617,R63,U563,L844,D63,R15,U634,R643,D124,L147,D583,R716,D28,L799,D59,R819,D723,L43,D975,L755,D635,R118,U325,L969,D445,R374,D797,L821,U118,R962,D643,R127,U267,R768,D50,L343,U80,R281,U575,R618,D718,L74,U146,R242,D547,L492,U71,R826,D483,L402,U953,R184,U707,L973,D550,L593,U281,L652,D247,L254,D60,R908,U581,L731,D634,R286,D186,R9,D983,L181,U262,R241,D674,R463,U238,R600"
        ]
        XCTAssertGreaterThan(distToClosestIntersection(filelines), 471)
        XCTAssertLessThan(distToClosestIntersection(filelines), 581)

    }
}

/// To start a day, copy and paste this line, replacing the day
@testable import day4

/// To start a day, copy and paste this test class
final class Day4Tests: XCTestCase {
    func testDupes() {
        XCTAssert(hasDupe("11012382"))
        XCTAssertFalse(hasDupe("1231232321"))
    }
    
    func testIncreasing() {
        XCTAssert(isIncreasing("123459"))
        XCTAssertFalse(isIncreasing("123454"))
    }
    
    func testGuesses() {
        XCTAssertNotEqual(countPasswords(356261...846303), 246)
    }
}

/// To start a day, copy and paste this line, replacing the day
@testable import day5

/// To start a day, copy and paste this test class
final class Day5Tests: XCTestCase {
    func testInput() {
        //let state1 = ProgramState(memory: [1002,4,3,4,33], pointer: 0, input: 0, output: 0)
        XCTAssertEqual(runIntcodeProgram([1002,4,3,4,33]), [1002,4,3,4,99])
        XCTAssertEqual(runIntcodeProgram([1101,100,-1,4,0]), [1101,100,-1,4,99])
    }
    
    func testBranches() {
        /// Using position mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
        let state1 = ProgramState(memory: [3,9,8,9,10,9,4,9,99,-1,8], input: 8)
        XCTAssertEqual(runIntcodeProgram(state1)?.output, 1)
        let state2 = ProgramState(memory: [3,9,8,9,10,9,4,9,99,-1,8], input: 7)
        XCTAssertEqual(runIntcodeProgram(state2)?.output, 0)
        
        /// Using position mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not)
        let state3 = ProgramState(memory: [3,9,7,9,10,9,4,9,99,-1,8], input: 8)
        XCTAssertEqual(runIntcodeProgram(state3)?.output, 0)
        let state4 = ProgramState(memory: [3,9,7,9,10,9,4,9,99,-1,8], input: 9)
        XCTAssertEqual(runIntcodeProgram(state4)?.output, 0)
        let state5 = ProgramState(memory: [3,9,7,9,10,9,4,9,99,-1,8], input: 7)
        XCTAssertEqual(runIntcodeProgram(state5)?.output, 1)
        
        let in3 = [3,3,1108,-1,8,3,4,3,99] //Using immediate mode, consider whether the input is equal to 8; output 1 (if it is) or 0 (if it is not).
        XCTAssertEqual(runIntcodeProgram(ProgramState(memory: in3, input: 8))?.output, 1)
        XCTAssertEqual(runIntcodeProgram(ProgramState(memory: in3, input: 7))?.output, 0)
        
        let in4 = [3,3,1107,-1,8,3,4,3,99] // Using immediate mode, consider whether the input is less than 8; output 1 (if it is) or 0 (if it is not).
        XCTAssertEqual(runIntcodeProgram(ProgramState(memory: in4, input: 8))?.output, 0)
        XCTAssertEqual(runIntcodeProgram(ProgramState(memory: in4, input: 7))?.output, 1)
        
        let in5 = [3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9]
        XCTAssertEqual(runIntcodeProgram(ProgramState(memory: in5, input: 8))?.output, 1)
        XCTAssertEqual(runIntcodeProgram(ProgramState(memory: in5, input: 0))?.output, 0)
        
        let in6 = [3,3,1105,-1,9,1101,0,0,12,4,12,99,1]
        XCTAssertEqual(runIntcodeProgram(ProgramState(memory: in6, input: 8))?.output, 1)
        XCTAssertEqual(runIntcodeProgram(ProgramState(memory: in6, input: 0))?.output, 0)
        
    }
    
    func testGuesses() {
        let input = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,59,58,224,1001,224,-3422,224,4,224,102,8,223,223,101,3,224,224,1,224,223,223,1101,59,30,225,1101,53,84,224,101,-137,224,224,4,224,1002,223,8,223,101,3,224,224,1,223,224,223,1102,42,83,225,2,140,88,224,1001,224,-4891,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,61,67,225,101,46,62,224,1001,224,-129,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1102,53,40,225,1001,35,35,224,1001,224,-94,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,1101,5,73,225,1002,191,52,224,1001,224,-1872,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,102,82,195,224,101,-738,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,83,52,225,1101,36,77,225,1101,9,10,225,1,113,187,224,1001,224,-136,224,4,224,1002,223,8,223,101,2,224,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1007,226,226,224,1002,223,2,223,1006,224,329,1001,223,1,223,1108,226,226,224,102,2,223,223,1006,224,344,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,359,101,1,223,223,1108,677,226,224,1002,223,2,223,1005,224,374,1001,223,1,223,7,677,226,224,102,2,223,223,1005,224,389,1001,223,1,223,1008,677,677,224,1002,223,2,223,1005,224,404,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,419,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,434,1001,223,1,223,1107,677,226,224,1002,223,2,223,1005,224,449,101,1,223,223,1008,226,226,224,102,2,223,223,1005,224,464,1001,223,1,223,8,226,226,224,1002,223,2,223,1006,224,479,1001,223,1,223,107,226,677,224,102,2,223,223,1005,224,494,1001,223,1,223,7,226,226,224,102,2,223,223,1005,224,509,1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,524,101,1,223,223,107,677,677,224,1002,223,2,223,1006,224,539,101,1,223,223,8,677,226,224,1002,223,2,223,1006,224,554,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,569,101,1,223,223,108,226,677,224,1002,223,2,223,1006,224,584,101,1,223,223,7,226,677,224,1002,223,2,223,1005,224,599,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,614,1001,223,1,223,108,677,677,224,1002,223,2,223,1006,224,629,1001,223,1,223,1007,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1108,226,677,224,102,2,223,223,1005,224,659,1001,223,1,223,1107,226,677,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226]
        let state = ProgramState(memory: input, input: 1)
        XCTAssertEqual(runIntcodeProgram(state)?.output, 7265618)
    }
}

