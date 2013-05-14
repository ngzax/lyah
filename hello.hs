main = do
  let x = foo 2
  putStrLn "Hello World"

test = proc _ -> do
           question <- ask -< "what is the question ?"
           answer   <- ask -< question
           returnA -< ("the answer to '" ++ question ++ "' is " ++ answer)

instance Monad ((->) r) where
  return = const
  f >>= k = \ r -> k (f r) r


