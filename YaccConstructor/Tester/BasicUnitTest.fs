﻿module BasicUnitTest

open NUnit.Framework

[<TestFixture>]
type ``RACC core tests`` () =    
    inherit RACCCoreTests.``RACC core tests``()


[<TestFixture>]
type ``Yard frontend tests`` () =    
    inherit YardFrontendTester .``YARD frontend Tests``()

[<TestFixture>]
type ``Irony frontend tests`` () =    
    inherit  IronyFrontendTests.``Irony frontend tests``()