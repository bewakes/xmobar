{-# LANGUAGE CPP #-}

module Plugins.GetIP(
      GetIP(..)
    , getCommandOutput
    , getIP
    , getSSID
) where

import Plugins
import System.Process

-- ls /sys/class/net
--
ifconfig int = getCommandOutput "ifconfig" [int] ""
--grepInet pat = getCommandOutput "grep" ["inet "] pat
grepIP = getCommandOutput "grep" ["-Eo", "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+"]

-- COMMANDS
nmcli_cmd = "nmcli d wifi"
grep_cmd patt = getCommandOutput "grep" [] patt
command_wifi_ssid = "nmcli d wifi | grep \\* | sed 's/ \\+/$/g' | cut -d '$' -f 2"

getCommandOutput :: String -> [String] -> String -> IO String
getCommandOutput cmd args stin = do
    (ec, o, er) <- readProcessWithExitCode cmd args stin
    return o

getInterface :: Char -> IO String
getInterface i = do
    op <- getCommandOutput "ls" ["/sys/class/net"] ""
    int <- return $ head $ filter (\x-> head x == i) $ words op
    return int

getSSID :: IO String
getSSID = do
    op <- getCommandOutput "iwgetid" ["-r"] ""
    return op
    {-grepped <- getCommandOutput "grep" ["\\*"] op-}
    {-sedded <- getCommandOutput "sed" ["s/ \\+/$/g"] grepped-}
    {-cut <- getCommandOutput "cut" ["-d", "$", "-f", "2"] sedded-}
    {-return cut -}

extractIp :: [String] -> String
extractIp [] = ""
extractIp (x:xs) = x

getIP i = do
    int <- getInterface i
    a <- ifconfig int
    b <- grepIP a
    name <- if i == 'w' then getSSID else return ""
    return $ extractIp (words b) ++ "(" ++ take (length name - 1) name ++ ")"

data GetIP = GetIP String String Int
    deriving (Read, Show)

instance Exec GetIP where
    alias (GetIP _ a _) = a
    run (GetIP i _ _) = getIP $ head i
    --frate (GetIP _ _ r) = r
