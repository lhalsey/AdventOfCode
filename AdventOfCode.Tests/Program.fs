open System.Globalization
open BenchmarkDotNet.Running
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open BenchmarkDotNet.Reports
open BenchmarkDotNet.Jobs
open BenchmarkDotNet.Columns
open Perfolizer.Horology
open AdventOfCode.Days.Y2019 // Change year to run other benchmarks

module Program =

    [<MemoryDiagnoser>] // Retrieves additional information about memory (Gen0, Gen1 and Gen2 allocations)
    type Benchmarks() =
        [<Benchmark>] member __.Day01_Part1() = Day01.Part1()
        [<Benchmark>] member __.Day01_Part2() = Day01.Part2()
        [<Benchmark>] member __.Day02_Part1() = Day02.Part1()
        [<Benchmark>] member __.Day02_Part2() = Day02.Part2()
        [<Benchmark>] member __.Day03_Part1() = Day03.Part1()
        [<Benchmark>] member __.Day03_Part2() = Day03.Part2()
        [<Benchmark>] member __.Day04_Part1() = Day04.Part1()
        [<Benchmark>] member __.Day04_Part2() = Day04.Part2()
        [<Benchmark>] member __.Day05_Part1() = Day05.Part1()
        [<Benchmark>] member __.Day05_Part2() = Day05.Part2()
        [<Benchmark>] member __.Day06_Part1() = Day06.Part1()
        [<Benchmark>] member __.Day06_Part2() = Day06.Part2()
        [<Benchmark>] member __.Day07_Part1() = Day07.Part1()
        [<Benchmark>] member __.Day07_Part2() = Day07.Part2()
        [<Benchmark>] member __.Day08_Part1() = Day08.Part1()
        [<Benchmark>] member __.Day08_Part2() = Day08.Part2()
        [<Benchmark>] member __.Day09_Part1() = Day09.Part1()
        [<Benchmark>] member __.Day09_Part2() = Day09.Part2()
        [<Benchmark>] member __.Day10_Part1() = Day10.Part1()
        [<Benchmark>] member __.Day10_Part2() = Day10.Part2()
        [<Benchmark>] member __.Day11_Part1() = Day11.Part1()
        [<Benchmark>] member __.Day11_Part2() = Day11.Part2()
        [<Benchmark>] member __.Day12_Part1() = Day12.Part1()
        [<Benchmark>] member __.Day12_Part2() = Day12.Part2()
        [<Benchmark>] member __.Day13_Part1() = Day13.Part1()
        [<Benchmark>] member __.Day13_Part2() = Day13.Part2()
        [<Benchmark>] member __.Day14_Part1() = Day14.Part1()
        [<Benchmark>] member __.Day14_Part2() = Day14.Part2()
        [<Benchmark>] member __.Day15_Part1() = Day15.Part1()
        [<Benchmark>] member __.Day15_Part2() = Day15.Part2()
        [<Benchmark>] member __.Day16_Part1() = Day16.Part1()
        [<Benchmark>] member __.Day16_Part2() = Day16.Part2()
        [<Benchmark>] member __.Day17_Part1() = Day17.Part1()
        [<Benchmark>] member __.Day17_Part2() = Day17.Part2()
        [<Benchmark>] member __.Day18_Part1() = Day18.Part1()
        [<Benchmark>] member __.Day18_Part2() = Day18.Part2()
        [<Benchmark>] member __.Day19_Part1() = Day19.Part1()
        [<Benchmark>] member __.Day19_Part2() = Day19.Part2()
        [<Benchmark>] member __.Day20_Part1() = Day20.Part1()
        [<Benchmark>] member __.Day20_Part2() = Day20.Part2()
        [<Benchmark>] member __.Day21_Part1() = Day21.Part1()
        [<Benchmark>] member __.Day21_Part2() = Day21.Part2()
        [<Benchmark>] member __.Day22_Part1() = Day22.Part1()
        [<Benchmark>] member __.Day22_Part2() = Day22.Part2()
        [<Benchmark>] member __.Day23_Part1() = Day23.Part1()
        [<Benchmark>] member __.Day23_Part2() = Day23.Part2()
        [<Benchmark>] member __.Day24_Part1() = Day24.Part1()
        [<Benchmark>] member __.Day24_Part2() = Day24.Part2()
        [<Benchmark>] member __.Day25_Part1() = Day25.Part1()
        [<Benchmark>] member __.Day25_Part2() = Day25.Part2()

    let [<EntryPoint>] main _ =

        let benchmarkJob =
            Job.Default
                .WithWarmupCount(1)
                .WithIterationTime(TimeInterval.FromMilliseconds(250.0))
                .WithMinIterationCount(10)
                .WithMaxIterationCount(15)
                .DontEnforcePowerPlan();

        let summaryStyle =
            new SummaryStyle(
                CultureInfo.CurrentCulture, true, SizeUnit.B, TimeUnit.Millisecond, false);

        let benchmarkConfig =
            DefaultConfig.Instance
                .AddJob(benchmarkJob.AsDefault())
                .WithSummaryStyle(summaryStyle);
        
        BenchmarkRunner.Run<Benchmarks>(benchmarkConfig) |> printfn "%A"
        0
