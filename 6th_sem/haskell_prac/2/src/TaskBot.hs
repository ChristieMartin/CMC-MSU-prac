module TaskBot
  ( runBot
  ) where

import Consts
import System.IO

type Task = String
type UserName = String
type BotName = String

-- | Run task bot.
-- Commands:
-- /list -- show task list
-- /complete -- complete the last task
-- /exit -- stop bot
-- Any other input is considered as new task.
runBot :: IO ()
runBot = do
  -- disable buffering for stdout
  hSetBuffering stdout NoBuffering
  putStrLn namePrompt
  name <- getLine
  putStrLn botPrompt
  botName <- getLine
  go name botName []
  where
    -- Helper function to interact with user and update tasks list
    go :: UserName -> BotName -> [Task] -> IO ()
    go name botName taskList = do
      putStr $ name ++ " > "
      str <- getLine
      if (str == "/exit")
        then putStrLn goodbyeMsg
        else do
          -- process input unless it is an "/exit" command
          let (output, newTaskList) = processCommand str taskList
          putStrLn (botName ++ " > " ++ output)
          go name botName newTaskList

-- | Process user input. Returns output string to be printed by bot and
-- updated list of tasks in a tuple.
processCommand :: String -> [Task] -> (String, [Task])
processCommand cmd prevTaskList = case cmd of
  "/list" -> cmdList prevTaskList
  "/complete" -> cmdComplete prevTaskList
  "/delete" -> delList prevTaskList
  _ -> addTask cmd prevTaskList

-- | Command to show tasks list.
cmdList :: [Task] -> (String, [Task])
cmdList tasks = (myShow 0 tasks, tasks)
  where
    myShow :: Int -> [Task] -> String
    myShow _ [] = []
    myShow 0 x = "\n" ++ myShow 1 x
    myShow numb (x:xs) = show numb ++ ". " ++ x ++ "\n" ++ myShow (numb + 1) xs

-- | Command to delete all comands
delList :: [Task] -> (String, [Task])
delList _ = (deleteMsg, [])

-- | Command to complete the last task.
cmdComplete :: [Task] -> (String, [Task])
cmdComplete [] = (noTasksMsg, [])
cmdComplete (_:xs) = (completeMsg, xs)

-- | Add new task to tasks list.
addTask :: String -> [Task] -> (String, [Task])
addTask task l = (newTaskMsg, task:l)
