-- Onur Yilmaz

-- Importing List and Char modules
import Data.List
import Data.Char

-- Data definition of StrStat
data StrStat = StrStat { charfreq :: [(Char,Int)], wordfreq :: [([Char],Int)], charcount :: Int, wordcount :: Int , parerror :: Maybe Int} deriving Show

-- "analyze" function
-- Main analysis function which combines all string statistics
analyze :: [Char] -> StrStat
analyze sentence = StrStat { charfreq = charfreqHelper sentence, wordfreq = wordfreqHelper sentence, charcount = charcountHelper sentence, wordcount = wordcountHelper sentence, parerror = parerrorHelper sentence }


-- General purpose functions
-- Splits the string according to alphanumeric check and returns a list of words
splitter [] = []
splitter " " = []
splitter sentence= [(fst(span isAlphaNum sentence))  ] ++ if length(snd(span isAlphaNum sentence)) > 0 then splitter (tail(snd(span isAlphaNum sentence))) else splitter []
splitterFiltered sentence = filter (/= "") (splitter sentence)


-- charfreqHelper function
-- This function returns the frequency of characters
-- Lambda function is taken from an example in learnyouahaskell.com
charfreqHelper sentence = map (\ (x:xs) -> (x, length (x:xs)))  (group (sort (fst (partition isAlphaNum sentence )))) 

-- wordfreqHelper function
-- varYok function checks if there are any exact words in the lists of words
-- howMany function creates the pairwise list 
varYokInt a b =  if a==b then 1 else 0
varYokList [] _ = 0
varYokList word kelimeler = sum (map (varYokInt word) kelimeler)
howMany a =  [ (x, varYokList x a) | x <- a ]
wordfreqHelper sentence = sort( nub (howMany (splitterFiltered sentence)))

-- charcountHelper function
-- This function maps length function to words list and sums all values
charcountHelper sentence =  sum(map length (splitterFiltered sentence))

-- wordcountHelper function 
-- This function returns the length of words list
wordcountHelper sentence =  length(splitterFiltered sentence)

-- mapper function
-- This function is used for mapping string to (integer, character) tuples
-- onlyParantez is used for eliminating tuples other than paranthesis
mapper [] a = [(' ',0)]
mapper sentence sayi = [(head sentence, sayi)] ++ mapper (tail sentence) (sayi+1)
onlyParantez sentence = [ (a,b) | (a,b) <- (mapper sentence 0), elem a "{([]})" ]

-- Getter functions for paranthesis tuples
firstInParaList list = fst (head list)
locationInString list = snd (head list)

-- stack function
-- This function implements a stack for paranthesis
-- Inputs: stack of expected paranthesis, length of string, paranthesis tuples
-- Output: Maybe Int
-- This first part is ued for situations where the stack is empty
-- If stack and string is empty, return Nothing
-- If stack is empty and closing paranthesis, return its location
-- If stacj is empty and opening paranthesis, if string is 1-length return its location, 
											  -- otherwise add to matching expected paranthesis to stack
stack "" [(a,b)] c = Just b
stackOfPara ""  c paraList
	| (length paraList) == 0 = Nothing 
	| (firstInParaList paraList) == ')' = Just (locationInString paraList)
	| (firstInParaList paraList) == ']' = Just (locationInString paraList)
	| (firstInParaList paraList) == '}' = Just (locationInString paraList)
	| (firstInParaList paraList) == '(' && (length paraList) == 1 = Just (locationInString paraList)
	| (firstInParaList paraList) == '[' && (length paraList) == 1 = Just (locationInString paraList)
	| (firstInParaList paraList) == '{' && (length paraList) == 1 = Just (locationInString paraList)
	| (firstInParaList paraList) == '(' && (length paraList)  > 1 = stackOfPara ")" c (tail paraList)
	| (firstInParaList paraList) == '[' && (length paraList) >  1 = stackOfPara "]" c (tail paraList)
	| (firstInParaList paraList) == '{' && (length paraList) > 1 = stackOfPara "}" c (tail paraList)
	| otherwise = Just c
	
-- This second part of function deals with the situations where the stack of expected paranthesis is not empty	
stackOfPara str c paraList
	| (head str)== ')' && (firstInParaList paraList) == ')' && (length paraList) == 1 && (length str) == 1  = Nothing
	| (head str)== ')' && (firstInParaList paraList) == ')' && (length paraList) > 1 = stackOfPara (tail str) c (tail paraList)	
	| (head str)== ')' && (length paraList) == 0 = Just c
	| (head str)== ')' && (firstInParaList paraList) == ']' = Just (locationInString paraList)
	| (head str)== ')' && (firstInParaList paraList) == '}' = Just (locationInString paraList)
	| (head str)== ')' && (firstInParaList paraList) == '(' && (length paraList) == 1 = Just (locationInString paraList)
	| (head str)== ')' && (firstInParaList paraList) == '[' && (length paraList) == 1 = Just (locationInString paraList)
	| (head str)== ')' && (firstInParaList paraList) == '{' && (length paraList) == 1 = Just (locationInString paraList)
	| (head str)== ')' && (firstInParaList paraList) == '(' && (length paraList)  > 1 = stackOfPara (")"++str)  c (tail paraList)
	| (head str)== ')' && (firstInParaList paraList) == '[' && (length paraList) >  1 = stackOfPara ("]"++str) c (tail paraList)
	| (head str)== ')' && (firstInParaList paraList) == '{' && (length paraList) > 1 = stackOfPara ("}"++str)  c (tail paraList)		
	| (head str)== ']' && (firstInParaList paraList) == ']' && (length paraList) == 1 && (length str) == 1 = Nothing
	| (head str)== ']' && (length paraList) == 0 = Just c
	| (head str)== ']' && (firstInParaList paraList) == ']' && (length paraList) > 1 = stackOfPara (tail str) c (tail paraList)
	| (head str)== ']' && (firstInParaList paraList) == ')' = Just (locationInString paraList)
	| (head str)== ']' && (firstInParaList paraList) == '}' = Just (locationInString paraList)
	| (head str)== ']' && (firstInParaList paraList) == '(' && (length paraList) == 1 = Just (locationInString paraList)
	| (head str)== ']' && (firstInParaList paraList) == '[' && (length paraList) == 1 = Just (locationInString paraList)
	| (head str)== ']' && (firstInParaList paraList) == '{' && (length paraList) == 1 = Just (locationInString paraList)
	| (head str)== ']' && (firstInParaList paraList) == '(' && (length paraList)  > 1 = stackOfPara (")"++str) c (tail paraList)
	| (head str)== ']' && (firstInParaList paraList) == '[' && (length paraList) >  1 = stackOfPara ("]"++str) c (tail paraList)
	| (head str)== ']' && (firstInParaList paraList) == '{' && (length paraList) > 1 = stackOfPara ("}"++str) c (tail paraList)	
	| (head str)== '}' && (firstInParaList paraList) == '}' && (length paraList) == 1 && (length str) == 1  = Nothing
	| (head str)== '}' && (length paraList) == 0 = Just c
	| (head str)== '}' && (firstInParaList paraList) == '}' && (length paraList) > 1 = stackOfPara (tail str) c (tail paraList)
	| (head str)== '}' && (firstInParaList paraList) == ')' = Just (locationInString paraList)
	| (head str)== '}' && (firstInParaList paraList) == ']' = Just (locationInString paraList)
	| (head str)== '}' && (firstInParaList paraList) == '(' && (length paraList) == 1 = Just (locationInString paraList)
	| (head str)== '}' && (firstInParaList paraList) == '[' && (length paraList) == 1 = Just (locationInString paraList)
	| (head str)== '}' && (firstInParaList paraList) == '{' && (length paraList) == 1 = Just (locationInString paraList)
	| (head str)== '}' && (firstInParaList paraList) == '(' && (length paraList)  > 1 = stackOfPara (")"++str) c (tail paraList)
	| (head str)== '}' && (firstInParaList paraList) == '[' && (length paraList) >  1 = stackOfPara ("]"++str) c (tail paraList)
	| (head str)== '}' && (firstInParaList paraList) == '{' && (length paraList) > 1 = stackOfPara ("}"++str) c (tail paraList)	
	| otherwise = Just (c+1)

-- parerrorHelper function
-- This function uses stack implementation which is provided above
parerrorHelper sentence = stackOfPara "" (length(sentence)-1) (onlyParantez sentence)

-- End of functions

-- End of code
-- April 3, 2012