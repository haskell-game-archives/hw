{-# LANGUAGE CPP #-}
module Logging where

data LogLevel
  = Debug
  | Warn
  | Error

log :: LogLevel -> String -> a -> a
#if defined(DEBUG)
log Debug s = trace ("DEBUG: " ++ s)
#endif
#if defined(WARN) || defined(DEBUG)
log Warn s = trace ("WARN: " ++ s)
#endif
#if defined(ERROR) || defined(WARN) || defined(DEBUG)
log Error s = trace ("ERROR: " ++ s)
#endif
#if !(defined(ERROR) && defined(WARN) && defined(DEBUG)) || !defined(DEBUG)
log _ _ = id
#endif

logIO :: LogLevel -> String -> IO ()
#if defined(DEBUG)
logIO Debug s = traceIO ("DEBUG: " ++ s)
#endif
#if defined(WARN) || defined(DEBUG)
logIO Warn s = traceIO ("WARN: " ++ s)
#endif
#if defined(ERROR) || defined(WARN) || defined(DEBUG)
logIO Error s = traceIO ("ERROR: " ++ s)
#endif
#if !(defined(ERROR) && defined(WARN) && defined(DEBUG)) || !defined(DEBUG)
logIO _ _ = return ()
#endif
