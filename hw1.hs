import Data.List

data Complex = Complex Double Double
data Qubit = Qubit {_0 :: Complex, _1 :: Complex}

instance Num Complex where
    (Complex i j) + (Complex k l) = Complex (i + k) ( j + l)
    (Complex i j) - (Complex k l) = Complex (i - k) ( j - l)
    (Complex i j) * (Complex k l) = Complex (i * k - j * l) (i * l + j * k)
    abs (Complex i j) = error "Abs not Possible"
    signum (Complex i j) = error "Signum not Possible"
    fromInteger a = Complex (fromIntegral a) 0.0

instance Show Complex where
    show (Complex i j)
        | i == 0 && j == 0 = "NP"
        | i == 0 = show(j) ++ "i"
        | j == 0 = show(i)
        | otherwise = show(i) ++ " " ++ show(j) ++ "i"

instance Show Qubit where
    show (Qubit zero one)
        | (show zero) == "NP" = "Current State: (" ++ show one ++ ")|1>"
        | (show one) == "NP"  = "Current State: (" ++ show zero ++ ")|0>"
        | otherwise           = "Current State: (" ++ show zero ++ ")|0> (" ++ show one ++ ")|1>"

power :: Complex -> Double -> Complex
power (Complex i j) k = Complex (i**k - j ** k) (i * j * 2)

mag :: Complex -> Double
mag (Complex i j) = i**2 + j**2 

measure :: Qubit -> String
measure (Qubit zero one) = "Probability of |0> : " ++ show(mag(zero)) ++ " and |1> of " ++ show(mag(one))

makeComp :: (Double, Double) -> Complex
makeComp (i, j) = Complex i j 

isItValid :: [Complex] -> Bool
isItValid [c1,c2] = (mag c1 + mag c2) > 0.9995 && (mag c1 + mag c2) < 1.0005
isItValid _ = False

c2Qubit :: [Complex] -> Qubit
c2Qubit [c1,c2]
    | isItValid [c1,c2] = Qubit c1 c2
    | otherwise = error "The vector is not normalized!"
c2Qubit _ = error "The vector is not normalized!"

hadamard :: Qubit -> Qubit
hadamard (Qubit zero one) = Qubit newzero newone
    where
        cons = Complex (1 / sqrt 2) 0
        newzero = zero * cons + one * cons
        newone  = zero * cons + one * (-1) *(cons) 

x_trans :: Qubit -> Qubit 
x_trans (Qubit zero one) = Qubit newzero newone
    where
        cons = Complex 1 0
        newzero = (one * cons)
        newone  = (zero * cons)

y_trans :: Qubit -> Qubit
y_trans (Qubit zero one) = Qubit newzero newone
    where
        p_cons = Complex 0 1
        newzero = one * p_cons
        newone = zero * p_cons

z_trans :: Qubit -> Qubit
z_trans (Qubit zero one) = Qubit newzero newone
    where
        p_cons = Complex 1 0
        newzero = zero * p_cons
        newone = one * (-1) * p_cons

doesItHave :: Char -> String -> Bool
doesItHave a (x:xs)
    | a == x = True
    | otherwise = doesItHave a xs
doesItHave a [] = False

makeDouble :: String -> Double
makeDouble a = read a :: Double

parseComplex :: (Double, Double) -> Complex
parseComplex (a,b) = Complex a b

parseStr :: String -> String -> [String]
parseStr (x:xs) temp
    | x == ','  = [temp] ++ parseStr xs []
    | otherwise = parseStr xs (temp ++ [x])
parseStr [] temp = [temp]

parseIValue :: String -> Double
parseIValue []   = 1.0
parseIValue "-"  = -1.0
parseIValue a    = makeDouble a

parseNum :: String -> [Double] -> String -> (Double, Double)
parseNum strtemp numtemp [] = (makeDouble strtemp, makeDouble "0")
parseNum strtemp numtemp (x:xs)
    | x == 'i' && real                         = (numtemp !! 0  , parseIValue strtemp)
    | x == 'i'                                 = (makeDouble "0", parseIValue strtemp)
    | doesItHave x "0123456789."               = parseNum (strtemp ++ [x]) numtemp  xs
    | doesItHave x "-" && (length strtemp) > 0 = parseNum [x] (numtemp ++ [makeDouble strtemp]) xs
    | doesItHave x "-"                         = parseNum [x] numtemp xs
    | doesItHave x "+" && (length strtemp) > 0 = parseNum [] (numtemp ++ [makeDouble strtemp]) xs
    | doesItHave x "+"                         = parseNum [] numtemp xs
    | otherwise                                = parseNum strtemp numtemp xs
    where
        real   = (length numtemp) > 0


makeQubit n = c2Qubit $ map parseComplex $ map (parseNum [] []) (parseStr n [])

-- take_inp :: Maybe Qubit
take_inp = do
    putStrLn "Enter the amplitudes of the initial state: "
    n <- getLine
    let check = isItValid $ map parseComplex (map (parseNum [] []) (parseStr n []))
    case check of
        False -> do
            putStrLn "Error : The vector is not normalized!"
            take_inp
        True -> do
            return $ Just $ makeQubit n

inLoop qubit = do
    putStrLn "Do you wish to continue (Y/N)? "
    exit_ <- getLine
    case exit_ of
        "Y" -> main
        "N" -> return ()
        _ -> do
            putStrLn "This is not what i expected!"
            inLoop qubit

state qubit = do
    putStrLn $ show qubit
    putStrLn $ "Enter the transformation: "
    action <- getLine

    case action of
        "M" -> do 
            putStrLn $ measure $ qubit
            inLoop qubit
        "H" -> do
            state $ hadamard qubit
        "X" -> do 
            state $ x_trans qubit
        "Y" -> do
            state $ y_trans qubit
        "Z" -> do
            state $ z_trans qubit
        _ -> do
            putStrLn "Transformation not recognized"
            state qubit

main = do
    Just qubit <- take_inp
    state qubit
    