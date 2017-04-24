{-# LANGUAGE CPP #-}

module Plugins.GetIP(GetIP(..)) where

import Plugins
import System.Process

-- ls /sys/class/net
--
ifconfig int = getCommandOutput "ifconfig" [int] ""
--grepInet pat = getCommandOutput "grep" ["inet "] pat
grepIP pat = getCommandOutput "grep" ["-Eo", "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+"] pat
--cut ip = getCommandOutput "cut" ["-d:", "-f2"] ip
--awk ip = getCommandOutput "awk" ["{print $1}"] ip

getCommandOutput :: String -> [String] -> String -> IO String
getCommandOutput cmd args stin = do
    (ec, o, er) <- readProcessWithExitCode cmd args stin
    return o

getInterface :: Char -> IO String
getInterface i = do
    op <- getCommandOutput "ls" ["/sys/class/net"] ""
    int <- return $ head $ filter (\x-> head x == i) $ words op
    return int

extractIp :: [String] -> String
extractIp [] = ""
extractIp (x:xs) = x

getIP i = do
    int <- getInterface i
    a <- ifconfig int
    b <- grepIP a
    --c <- cut b
    --d <- awk c
    return $ extractIp $ words b

data GetIP = GetIP String String Int
    deriving (Read, Show)

instance Exec GetIP where
    alias (GetIP _ a _) = a
    run (GetIP i _ _) = getIP $ head i
    --frate (GetIP _ _ r) = r
