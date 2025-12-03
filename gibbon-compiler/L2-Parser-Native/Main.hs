import System.Environment (getArgs)
import RunTest (setConfig, printTest, Result(Success, Failure))

main = do 
    args <- getArgs
    let config = setConfig args
    print args
    case config of
        Failure e -> putStrLn $ "Error in command line arguments: " ++ e
        Success cfg -> printTest cfg