import HunitTest (htest)
import QuickTest (qtest)

main :: IO ()
main = htest >> qtest
