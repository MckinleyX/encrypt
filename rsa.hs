main = putStrLn "You probably want to run this via the interactive compiler..."

-- p and q are both large primes
-- we want p and q to be far apart so that
-- the modulus can't be trivially factored with
-- a^2-b^2=(a+b)(a-b)
p = 521821384516611830490897417105308476818528832887754927003992253994731222969439839154459580268776067196631395736842609082427753362083201158583773903165333706741154019850285487518620863262271880573154583828162999798086395901771541095540166352838686552709949067587194461821314789939070094516539903352287
q = 5429469469476712785705036445388937164808436768581126409657605751277450779760693801664855208023492971907679097395580801380461119907224163404808796444248478010375392383323050817032627066800557744472860685219973714683087623696166154966252612660288291503753016357891418932588195004047

-- modulus, part of both the private and public key
modulus = p*q

-- Exponent of public key
pub_exp :: Integer
pub_exp = 65537

-- Exponent of private key
pri_exp :: Integer
pri_exp = invertMod (tot p q) pub_exp
-- has the property that (a^e)^d = 1 (mod m) for any message a

-- Carmichael's Totient Function
-- The minimum number n such that a^n = 1 (mod modulus)
tot p q = lcm (p-1) (q-1) 

-- Find n^-1 (mod n) via Extended Euclidean Algorithm
-- since finding an = 1 (mod m) is the same as finding
-- a solution to an + bm = 1
-- Taken from https://gist.github.com/trevordixon/6895802
-- takes input as modulus, num
invertMod m n = snd (eea m n)
    where eea a 0 = (1, 0)
          eea a b = (t, s - q * t)
             where (q, r) = quotRem a b
                   (s, t) = eea b r

-- Modular exponentiation
-- Runs in O(log n)
modExp :: Integer -> Integer -> Integer -> Integer
-- modulus, exponent, num
modExp m e n 
   | e == 1         = n
   | e `mod` 2 == 1 = (n * modExp m (e-1) n) `mod` m
   | otherwise      = modExp m (e `div` 2) ((n*n) `mod` m)

encrypt = modExp modulus pub_exp

decrypt = modExp modulus pri_exp

