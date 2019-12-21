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
        let state2 = ProgramState(memory: input, input: 5)
        XCTAssertEqual(runIntcodeProgram(state2)?.output, 7731427)
    }
}

/// To start a day, copy and paste this line, replacing the day
@testable import day6

/// To start a day, copy and paste this test class
final class Day6Tests: XCTestCase {
    func testDirectAndIndirect() {
        XCTAssertEqual(sumChildrenOf(OrbitDictionary()), 0)
        let in1 = ["COM)A"].reduce(OrbitDictionary(), createOrbits)
        XCTAssertEqual(sumChildrenOf(in1), 1)
        let sample = [
            "COM)B",
            "B)C",
            "C)D",
            "D)E",
            "E)F",
            "B)G",
            "G)H",
            "D)I",
            "E)J",
            "J)K",
            "K)L",
        ].reduce(OrbitDictionary(), createOrbits)
        XCTAssertEqual(sumChildrenOf(sample), 42)
    }
    
    func testTransfers() {
        let in1 = [
            "COM)A",
            "A)SAN",
            "A)YOU"
        ].reduce(OrbitDictionary(), createOrbits)
        XCTAssertEqual(sumOrbitalTransfers(in1), 0)
        let in2 = [
            "COM)B",
            "B)C",
            "C)D",
            "D)E",
            "E)F",
            "B)G",
            "G)H",
            "D)I",
            "E)J",
            "J)K",
            "K)L",
            "K)YOU",
            "I)SAN",
        ].reduce(OrbitDictionary(), createOrbits)
        XCTAssertEqual(sumOrbitalTransfers(in2), 4)
    }
}

/// To start a day, copy and paste this line, replacing the day
@testable import day7

/// To start a day, copy and paste this test class
final class Day7Tests: XCTestCase {
    func testP1Samples() {
        XCTAssertEqual(
            findLargestThrusterSignal([3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]),
            43210)
    }
    
    func testP2Samples() {
        let in1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
                   27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
        XCTAssertEqual(findLargestThrusterSignalWithFeedback(in1), 139629729)
        let in2 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
                   -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
                   53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]
        XCTAssertEqual(findLargestThrusterSignalWithFeedback(in2), 18216)
    }
}

@testable import day8

final class Day8Tests: XCTestCase {
    func testp1Guesses() {
        let in1 = "220212222222222222222222212222222222222222220222022222021202222122212222120202222222222222220222022022222222202202212022222200222222222202222212212022222222222222222222222222202222222222222222221222022222122212222022202222020222222222222222220222222222222222222202212022222210222222222202222212202022222222222222222222222222212222222222222222221222122222020212222122222222121212222222222222220222022022222222222212212022222202222222222202222202212022221202222222222222222222222222222222222222220220222222221222222022202222222202222222222222220221122122222222222222212222222201222222222202222202202022220202222222222222222222222222222222222222222222022222120212222122212222021202222222222222222221022022222222222212202122222221222222222202222212202122222222222222222222222222212222222222222222220220022222120202222222212222120222222222222222221222222222222222222212222222222202222222222222222212202122220202222222222222222222202222222222222222222222222222122202222022222221022202222222222222221220222122222222201212222022222201222222222202222202202222222202222222222222222222212222222222222222222220222222221212222122202220221222222222222222222221122022222222200202222022222201222222222212222212222022221222222222222222222212212222222222222222222222022222222222222122212221121222222222222222221220122122222222210202212122222211222222222222222222212222221212222222222222222202202222222222222222222221222222222202222122212220020212222222222222221222022222222222212202202022222200222222222202222202222122221202222222222222222212202222222222222222221222022222121222222022222221220202222222222222221222022222222222211202202122222211222212222202222112222022222202222222222222222222212222222222222222222222022222021222222022202222020222222222222222221221122122222222210212222222222201022202222202222122202222222222222222222222222212212222202222222222220221222222020202222222202222121212222222222222220221122022222222221202202222222212222202222212222012222022220202222222222122222212222222212222222222221221022222220212222022212222221212221222222222222222122122222222201202202222222201222222222212222112202222222202222222222122222202202222212222222222222221022222021202221222202222221202220222222222222222122022202222201212202122222222222212222212222202202122220222222222222222222212212222220222222222220222022222222212221022222222222212222222222222220222122222212222200212202022222211222222222222222102212222220222222222222122222202212222202222222222222222022222021202221222202220022222220222222222220220022222222222222222222122222212222202222202222222222222221222222222222022222212222222222222222222220221022222120212222222222220122202221222222222221220222122212222200222212120222221122202222222222012222222221202222222222022222222222222202222222222220222022222021222221122202222121202222222222222220222222122222222200212212222222212222202222222222222222022220202222222222022222222212222211222222222222200022222121202221122222222221222020222222222201220022122222222211222202021222211222222222202222222212022222202222222222122222212202222212222222222221202122222121222220022212220121212222222222222212221022022202222222212212020222221022202222212222102222022220202222222222122222202222222202222222222222200022222222202222122222221022212022222222222121220022022202222221212222220222212222202222222222202222122220202222222222122222202202222222222222222220210122222020212220222212221020202121220222222011222022022202222202222212122222211022212222222222002212122202222222222222222222202212222200222222222220212022222122212222022202222120222120221222222110222222122212222211212222021202212122222222212222122202222221222222222220222222202222222200222222222222201222222122202212022202222121222120220222222212220122122212222221212202121202201022222222212222102212022210222222222220022222202212222201222222222220201222222121222211122202220022222221222222222201221122022222222201212202120222222222222222212222022212222210202222222222022222212222222201222222222222201122222211202220022202220121212121222222222110220222122202222221222012120202201222212222222222222012022201212222222220222222222222222212222222222220221222222120222221222222221222202222221222222111222122122212222220222212022202010022212222212222212122222212212222222221022222212212222212222222222220220222222111212212022202220221211022222222222111222222222202222200212122221222011022222222212222022212122200222222222221122222222212222211222222222222200022222100212210022212221100222220220222222222221122122222222211222112120222122022202222222222122222022211202222222221122222222222222210222222222220201222222110212221022202221120220220221022222102220222022222222210202002122212112122222222222222022022120211212222222220122222202212222222222222022222220122222110202220122212222201210122222022222000220122022222222220222122220222002222212222222222102110020211202222222221122222202202222220222222022222211022222112202222022212222102211120222122222011220220122222220202202022221222201022222222222222002020121212202222222220022222202222222202222222122222221122222010202220122212220011210122220022222222221020222202222201222202220222012222202222212222112222122222212222222222222222222222222221222222222222222022222010212210222212222000222121222022222212222220122222222212222022022202101022212222222222202120022221222222222221122222222202222202222222122221202222222001222202222202220110021020221222222002222020222212221211212222222212110022212222202222202212022210202222222220122222222212222200222222022221202222222220222211122202220220120221221222222010222121122202222221212222120222010022022222212222122102122221212222222221022222222212222222222202022222200022222222212202022212221210112121220022222220222221122202222202212012021202211122222222222222202000022211222222222221022222212202222220222222122221201122222211222220022222221022022221220022222111220022022202221221222012121222211022212222202222202112022222222222222222222222222212222200222202022220202122222021202221222212221010111220221022222001221121122220222222222122022222211222202222212222102220120212212222222221222222202222222221222202122220201022222200222221222222220020021122220122222011220020022200220221212002020222122222212222212222102012020221212222222221222222202222222212222212122222202222202010212200222202222122100021220222222221221220022220222202202012220202212220212222212222212211222211222222222222122222202222222202222212022220212222112111202202222222222121011221220122222100222022022201220202212212020222222020002222202222222100020222222212222221222222212222222211222202022222222122012220212211022212221210111220222222222020220020222202221201222212021222100120102222212222012202022201212222222220122221212212222200222202122220220222022020212211122202221012002220222122222002221020022211221211222202222222222220122222220222102000122211202222222222122222222012222212222212122220200022122012222201222122220222010222222222222222220020222202221221212102020222012121222222220222122020122221222202222221022220222202222201222202022222200122022021202210122122222000100121222022222101220222220212222201202112122202212022112222222222212112020200222212222221122222202112222200222222022221211122212102202221222212221110012122222022222100221220221210221202212202121212222122222222220222222202020222212202222220222220222122222212222201022222211022202122222211022102221202001222220122222122221120020202220211212202121222002122202222220222202012221202211212222221022222212122222210222201222221220022112002212210222212221102112221220022222101222222221222220201202202120222212022012222220222012120122220210202222220122222212012222210222201222221202122102111202210022112222202011220222222222210222222021200222200222002120202100220222222211222002001121211212202222221122222212112222201222201222220200222012221202211222122220212000122220022222111222120122221222202222202202212221020202222222222212212221211202222222222022221212202222201222210122221212122202120202202122102222111200121222122222022220120221212222201202012002222221220212222221222202021020211212202222220122222222222022220202220022221202122012102222212222002221221022021221022222220220220122200221222202020110210120121002222202222222110222221212222222220121222212022122210222212122220211222212012222212222202220002021221221122222221222022122200220220212112221200020021022222221222202120022222210212222222020222202212122220212210122221211222112110202200122112220112012222220222222100221221221200220210222112101211001020202222202222211022021212211222222220121221212222122221212201022221212222222210202200022022222120220120221122222212220220021200222221202020122211220020012222211222111021021220212202222221020220222122022200202212022222210022012002222210022122221002202021222122222000220220021200120221212220112210010021222222202222122102121212211222222222220222222102122211202221222220220222112000212220122102222101210122220222222022221122122221200210222022022201001021212222221222201002121210200202222220221220212002021221212222122221200122122200222212022202220222122120222220222102222220120221112212001112100220222120012222201222121022221202210222222220120222212202121211222211222222221222112212222210022112221002111221221021222201221022220200001211210002202220020122002222212222021200222200220202222220020222212012121200202221122221210022002201202210222002222020210021221022022012220120022202021202220002021211001121222222211222102200121221222222222222122222202222120200212210222220200122202221222211022000220020122021222121022200220220222211101200120001212200002220202222221222022122020201221222222221220221212002022221202200222221212222002110212210122000220120110122221122022002222022220201222221220012200202111120122222202222022111022222212202222220220220212122021221222210022221210122212011212202022020222100112221220221122000220221221210220202021210102211211221222222222222100212122201210212222220021222212112020210212211022222220022002001212121122112220221102121222020222120220221020212022220120220001201211121002222222222211101021211200212222220020220212102122211212220222220212122212000202201222011221020011021221021122111222220021220020202102012121102202210011212222222202120222211202222222220121220222012222202212200222220202222102021202100202022220022202021222022122220221122222211220221020211100011221122201212212222002111120222212222222221211222222012201212212222022222212122022200202112002112220202111022222120222022221122021201022222022120001101011021121202212221111102020221211202222222102220202112102211212220222222221022102011202121222212220200110220222021122222220122220211102210112012022021211100101202202220021212222210200222222221110221222222020210222222122222220222102011222220212122222020110120222221020100222022121201120212211020102220211111120202210221002011021211220202222222111220212022021221202212222220210122212200202200102220222000102220201221120221222220222201202210011000111012002220222222211221220112121210220202222222121221222012100220212200122220200022222222212102202010220010021220222121121210221020120212212211111220000120121111120202202221222012020220210212222221110222212202022220202222222221211222102101222222022002221021210221222121021002022120121220111211120211001110212121110212221220201000020210220212222220101221202022011222202210122222222122122120212122222220220210210221212021120002221221121221002202112122112011202120121112210222011002120202202202222220001220202102022210202222022221212022212122222220122212221122201021221210022220122122020212011200111210110022110102001012200122210122121212200212222222012221202022111201202000222220211222222012202210112022222201220221202000221210021120122210012220200200201202111210122122212022000220022202221212222220121221212202120222222110122220202022212011202001101021222221112221202021222121122221122210022200021011002210021120020122202020111200020221222202222220220221222022010222222000222221212022022101202020222200222212220121200122022002121021221221122220002110021000022021010022210020012011220220212212222221202220212102000211202100122221110122002220212210212222221120000121202211021111221022122210100212022121021210022021000002220222212200220200200212222221012220212002201200202112122220021122102120212210022101220002112022222011020122101220022211221211202022212102100020211212210022121102120221201212222221221222212122000201202202122221110122012022212210020101221001012122221101020222120022121201101202020202121001120202102202222021012211221202222202222220012222202002201210212022122200122122212120222210212010221222220020211221121111012022222210202200201221002101220222211122201221210112122221212212222220002221202222101221222110022202000122012200212020001010222002110222222201020210020022220221111220000100102011112110212002200201021011122201202202222221110220202212101222212021122201011122112020212120000221221000022220201101022022101220120220100202021220220222221200211222200010200112022201200222222221100222212012122211222002022211102122202202202100020022220101221021201211022110220022221211101202221220221101210201020002211100221110020200221202222222112220202012110220212001222222200222222220212100001100221201201220212122212212200220021210101201221200010212021121212212222210202220122220200222222222102222222022210202212100122211200022222122222201112210221212110121212221122200202121121211110202021211200200100100112022210011212111122220202222222222210220212112020200202202022210012022012122202220202221222202000120210101112111022220020210022200011212221102120102112112202121200010122222210202222221110221212102020221212202222202010022002021212112202200220022020001211112222000020021220201000222012002011202222211020212221111112121221212222212222221022221202202201220222211122201122022012020202001120202220122222100202002022100020021021202202222102100221220020221122102220212002221022212210202222220200221202002201210222210222221110122222020222012111120222001002211201220202220022021202202121202011020102222111002220002200101222222022221211202222222021222202222020220202101122200111222112011222001120112222122020102202002002120201122011212102222001110010102122212020012200111100122121212222202222221010221212222110222202001220200011022002220202111111022221102202000211200212012122120111210201222121120022110222020211102220120020222021211222222222222200221202222020212212210021211120022102212212212100200220210010120201010012011220220011222000212011111222210122102000212222201010001222200220212222220200222222102002201212211120211002122202221222010011110222120122211200211022212211122021222202201021022222200000202021222221010120011021221221212222221001120102202122211212121022211101022202211212221111211221112221001200102102010001220120200022222012111120212222111001012221221201011122220020122220222100221212222011222212110220212202222102121212021222112222110210212220201220110212022002220111201210212100001000220021122210222120011102212000202222220212222002022221211012002020212012022222011222112111220220101121021212101210120202121000222022222020221101120001010220112210111102021002122021011011010100100221020102101011222212012120200121111100111120001011210100101012201111001110201020101122001011000001200120101101120121021100210212"
        XCTAssertEqual(getMagicNumber(layers(in1[...])), 2193)
    }
}

@testable import day9

final class Day9Tests: XCTestCase {
    func testSamples() {
        let in1 = [1102,34915192,34915192,7,4,7,99,0]
        let p1 = ProgramState(memory: in1)
        XCTAssertEqual(String(runIntcodeProgram(p1)?.output ?? Int.min).count, 16)
        let p2 = ProgramState(memory: [104,1125899906842624,99])
        XCTAssertEqual(runIntcodeProgram(p2)?.output, 1125899906842624)
    }
    
    func testQuine() {
        let in3 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
        let p3 = ProgramState(memory: in3)
        XCTAssertEqual(runIntcodeProgram(p3)?.outputs, in3)
    }
    
    let input = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,3,0,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,1,37,1000,1101,856,0,1029,1101,286,0,1025,1101,39,0,1004,1101,861,0,1028,1101,845,0,1026,1102,28,1,1002,1102,1,0,1020,1101,0,892,1023,1101,0,291,1024,1101,35,0,1018,1101,0,27,1006,1102,1,26,1011,1101,33,0,1019,1102,31,1,1014,1102,1,36,1010,1102,23,1,1007,1101,0,32,1016,1101,29,0,1008,1101,20,0,1001,1102,1,25,1015,1101,38,0,1017,1101,0,24,1012,1102,1,22,1005,1101,1,0,1021,1101,0,21,1003,1102,1,838,1027,1102,1,30,1013,1101,895,0,1022,1101,0,34,1009,109,7,1208,0,22,63,1005,63,201,1001,64,1,64,1105,1,203,4,187,1002,64,2,64,109,-6,2102,1,5,63,1008,63,24,63,1005,63,223,1105,1,229,4,209,1001,64,1,64,1002,64,2,64,109,17,21102,40,1,-6,1008,1012,40,63,1005,63,255,4,235,1001,64,1,64,1106,0,255,1002,64,2,64,109,-15,21108,41,41,9,1005,1012,277,4,261,1001,64,1,64,1106,0,277,1002,64,2,64,109,11,2105,1,10,4,283,1105,1,295,1001,64,1,64,1002,64,2,64,109,-9,21101,42,0,8,1008,1013,44,63,1005,63,315,1105,1,321,4,301,1001,64,1,64,1002,64,2,64,109,13,1206,3,337,1001,64,1,64,1106,0,339,4,327,1002,64,2,64,109,-10,1208,0,29,63,1005,63,361,4,345,1001,64,1,64,1106,0,361,1002,64,2,64,109,2,2108,27,-4,63,1005,63,383,4,367,1001,64,1,64,1105,1,383,1002,64,2,64,109,-4,1207,2,30,63,1005,63,405,4,389,1001,64,1,64,1105,1,405,1002,64,2,64,109,22,1205,-8,417,1106,0,423,4,411,1001,64,1,64,1002,64,2,64,109,-27,2108,19,0,63,1005,63,443,1001,64,1,64,1106,0,445,4,429,1002,64,2,64,109,13,21108,43,45,-1,1005,1013,461,1106,0,467,4,451,1001,64,1,64,1002,64,2,64,109,1,21107,44,45,4,1005,1019,485,4,473,1105,1,489,1001,64,1,64,1002,64,2,64,109,-8,2102,1,-7,63,1008,63,37,63,1005,63,515,4,495,1001,64,1,64,1106,0,515,1002,64,2,64,109,1,2107,38,-4,63,1005,63,533,4,521,1105,1,537,1001,64,1,64,1002,64,2,64,109,4,21107,45,44,1,1005,1013,553,1106,0,559,4,543,1001,64,1,64,1002,64,2,64,109,-7,2107,21,-4,63,1005,63,575,1106,0,581,4,565,1001,64,1,64,1002,64,2,64,109,9,1205,7,599,4,587,1001,64,1,64,1105,1,599,1002,64,2,64,109,-11,2101,0,-3,63,1008,63,40,63,1005,63,619,1105,1,625,4,605,1001,64,1,64,1002,64,2,64,109,1,2101,0,-2,63,1008,63,28,63,1005,63,651,4,631,1001,64,1,64,1106,0,651,1002,64,2,64,109,1,21102,46,1,7,1008,1012,44,63,1005,63,671,1106,0,677,4,657,1001,64,1,64,1002,64,2,64,109,4,1201,-7,0,63,1008,63,28,63,1005,63,699,4,683,1105,1,703,1001,64,1,64,1002,64,2,64,109,-6,1207,-3,36,63,1005,63,719,1105,1,725,4,709,1001,64,1,64,1002,64,2,64,109,-4,1201,6,0,63,1008,63,23,63,1005,63,745,1106,0,751,4,731,1001,64,1,64,1002,64,2,64,109,8,1202,-6,1,63,1008,63,20,63,1005,63,777,4,757,1001,64,1,64,1105,1,777,1002,64,2,64,109,5,1202,-5,1,63,1008,63,25,63,1005,63,801,1001,64,1,64,1105,1,803,4,783,1002,64,2,64,109,8,21101,47,0,-6,1008,1014,47,63,1005,63,829,4,809,1001,64,1,64,1106,0,829,1002,64,2,64,109,1,2106,0,6,1001,64,1,64,1106,0,847,4,835,1002,64,2,64,109,11,2106,0,-4,4,853,1105,1,865,1001,64,1,64,1002,64,2,64,109,-15,1206,3,883,4,871,1001,64,1,64,1106,0,883,1002,64,2,64,109,14,2105,1,-8,1105,1,901,4,889,1001,64,1,64,4,64,99,21102,1,27,1,21102,1,915,0,1106,0,922,21201,1,57564,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,1,942,0,1105,1,922,22101,0,1,-1,21201,-2,-3,1,21101,957,0,0,1105,1,922,22201,1,-1,-2,1106,0,968,21202,-2,1,-2,109,-3,2106,0,0]
    func testPart1() {
        let p = ProgramState(memory: input, input: 1)
        let out = runIntcodeProgram(p)
        XCTAssertNotNil(out)
        XCTAssertEqual(out?.outputs.count, 1)
    }
    
    func testPart2() {
        let p = ProgramState(memory: input, input: 2)
        let out = runIntcodeProgram(p)
        XCTAssertNotNil(out)
        XCTAssertEqual(out?.outputs.count, 1)
    }
    
}

@testable import day10

final class Day10Tests: XCTestCase {
    func testSlope() {
        let p1 = Point(1, 1)
        let p2 = Point(2, 2)
        XCTAssertEqual(slope(p1, p2), Point(1, 1))
        let p3 = Point(0, 0)
        XCTAssertEqual(slope(p1, p3), Point(-1, -1))
        let p4 = Point(4, 4)
        XCTAssertEqual(slope(p1, p4), Point(1, 1))
        let p5 = Point(0, 4)
        XCTAssertEqual(slope(p5, p4), Point(1, 0))
        XCTAssertEqual(slope(p2, p5), Point(-1, 1))
        let p6 = Point(4, 0)
        XCTAssertEqual(slope(p6, p4), Point(0, 1))
        let p7 = Point(4, 6)
        XCTAssertEqual(slope(p3, p7), Point(2, 3))
        XCTAssertEqual(slope(p7, p3), Point(-2, -3))
        let p9 = Point(4, 0)
        XCTAssertEqual(slope(p2, p9), Point(1, -1))
    }
    
    func testPointsBetween() {
        let p1 = Point(1, 1)
        let p2 = Point(4, 4)
        XCTAssertEqual(pointsBetween(p1, p2), [Point(2,2), Point(3,3)])
    }
    
    func testExamples() {
        [
            (8, """
        .#..#
        .....
        #####
        ....#
        ...##
        """),
            (33, """
        ......#.#.
        #..#.#....
        ..#######.
        .#.#.###..
        .#..#.....
        ..#....#.#
        #..#....#.
        .##.#..###
        ##...#..#.
        .#....####
        """),
            (35, """
        #.#...#.#.
        .###....#.
        .#....#...
        ##.#.#.#.#
        ....#.#.#.
        .##..###.#
        ..#...##..
        ..##....##
        ......#...
        .####.###.
        """),
            (41, """
        .#..#..###
        ####.###.#
        ....###.#.
        ..###.##.#
        ##.##.#.#.
        ....###..#
        ..#.#..#.#
        #..#.#.###
        .##...##.#
        .....#.#..
        """),
            (210, """
        .#..##.###...#######
        ##.############..##.
        .#.######.########.#
        .###.#######.####.#.
        #####.##.#.##.###.##
        ..#####..#.#########
        ####################
        #.####....###.#.#.##
        ##.#################
        #####.##.###..####..
        ..######..##.#######
        ####.##.####...##..#
        .#####..#.######.###
        ##...#.##########...
        #.##########.#######
        .####.#.###.###.#.##
        ....##.##.###..#####
        .#.#.###########.###
        #.#.#.#####.####.###
        ###.##.####.##.#..##
        """)
            ]
            .forEach { answer, input in
                let map = toMap(input.split(separator: "\n"))
                XCTAssertGreaterThan(findBestMonitoringStationsSeenAsteroids(map), 8)
            }
    }
    
    func testGuesses() {
        let input = """
        #.#.###.#.#....#..##.#....
        .....#..#..#..#.#..#.....#
        .##.##.##.##.##..#...#...#
        #.#...#.#####...###.#.#.#.
        .#####.###.#.#.####.#####.
        #.#.#.##.#.##...####.#.##.
        ##....###..#.#..#..#..###.
        ..##....#.#...##.#.#...###
        #.....#.#######..##.##.#..
        #.###.#..###.#.#..##.....#
        ##.#.#.##.#......#####..##
        #..##.#.##..###.##.###..##
        #..#.###...#.#...#..#.##.#
        .#..#.#....###.#.#..##.#.#
        #.##.#####..###...#.###.##
        #...##..#..##.##.#.##..###
        #.#.###.###.....####.##..#
        ######....#.##....###.#..#
        ..##.#.####.....###..##.#.
        #..#..#...#.####..######..
        #####.##...#.#....#....#.#
        .#####.##.#.#####..##.#...
        #..##..##.#.##.##.####..##
        .##..####..#..####.#######
        #.#..#.##.#.######....##..
        .#.##.##.####......#.##.##
        """
        let map = toMap(input.split(separator: "\n"))
        let answer = findBestMonitoringStationsSeenAsteroids(map)
        XCTAssertGreaterThan(answer, 236)
        XCTAssertLessThan(answer, 270)
    }
}


@testable import day11

final class Day11Tests: XCTestCase {
    func testProgram() {
        let input = [3,8,1005,8,284,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,28,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,50,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,0,8,10,4,10,1001,8,0,72,1006,0,24,1,1106,12,10,1006,0,96,1,1008,15,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,108,1006,0,54,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,1,10,4,10,101,0,8,134,3,8,1002,8,-1,10,1001,10,1,10,4,10,108,1,8,10,4,10,1002,8,1,155,1006,0,60,1006,0,64,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,102,1,8,183,1006,0,6,1006,0,62,3,8,1002,8,-1,10,101,1,10,10,4,10,108,0,8,10,4,10,1002,8,1,211,1,108,0,10,2,1002,15,10,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,0,10,4,10,1001,8,0,242,3,8,102,-1,8,10,1001,10,1,10,4,10,108,0,8,10,4,10,1002,8,1,263,101,1,9,9,1007,9,1010,10,1005,10,15,99,109,606,104,0,104,1,21101,0,666526126996,1,21101,301,0,0,1105,1,405,21101,846138811028,0,1,21101,312,0,0,1106,0,405,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21101,0,248129978391,1,21101,359,0,0,1105,1,405,21101,97751403560,0,1,21102,1,370,0,1106,0,405,3,10,104,0,104,0,3,10,104,0,104,0,21101,988753585000,0,1,21101,393,0,0,1105,1,405,21102,867961709324,1,1,21102,404,1,0,1106,0,405,99,109,2,22102,1,-1,1,21102,40,1,2,21101,436,0,3,21102,1,426,0,1105,1,469,109,-2,2106,0,0,0,1,0,0,1,109,2,3,10,204,-1,1001,431,432,447,4,0,1001,431,1,431,108,4,431,10,1006,10,463,1102,0,1,431,109,-2,2106,0,0,0,109,4,1202,-1,1,468,1207,-3,0,10,1006,10,486,21102,1,0,-3,22101,0,-3,1,21202,-2,1,2,21102,1,1,3,21101,505,0,0,1106,0,510,109,-4,2106,0,0,109,5,1207,-3,1,10,1006,10,533,2207,-4,-2,10,1006,10,533,22101,0,-4,-4,1105,1,601,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,1,552,0,1105,1,510,21202,1,1,-4,21102,1,1,-1,2207,-4,-2,10,1006,10,571,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,593,21202,-1,1,1,21102,1,593,0,106,0,468,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0]
        day11.countPaintedPanels(input)
    }
}
