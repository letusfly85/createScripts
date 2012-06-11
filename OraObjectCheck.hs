module OraObjectCheck where

import Data.List

tablePrefixList = ["MS_","TR_","TP_","WK_","RR_","KR_"]
indexPrefixList = ["IX","PK_","IXT"]
procPrefixList  = ["PKGH","PKGB"]
seqPrefixList   = ["SQ_"]

getOraObjType :: String -> Either (String,String) (String,String)
getOraObjType str | any (\x -> x) (map (\x -> x `isPrefixOf` str) tablePrefixList) = Right (str,"table")
                  | any (\x -> x) (map (\x -> x `isPrefixOf` str) indexPrefixList) = Right (str,"index")
                  | any (\x -> x) (map (\x -> x `isPrefixOf` str) procPrefixList)  = Right (str,"procedure")
                  | any (\x -> x) (map (\x -> x `isPrefixOf` str) seqPrefixList)   = Right (str,"sequence")
                  | otherwise   = Left  (str,"NOT regular name")

getAllOraObjType :: [String] -> [Either (String,String) (String,String)]
getAllOraObjType [] = []
getAllOraObjType xs = foldr (\x ys -> (getOraObjType x) : ys) [] xs
