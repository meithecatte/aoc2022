#!/bin/bash
cp RatFunc.hs Prog.hs
sed -E '/^humn/d;s/: / = /g;s/root = (\w+) . (\w+)/root = (\1, \2)/' >> Prog.hs
echo 'root :: (RatFunc, RatFunc)' >> Prog.hs
echo 'main = print $ solve root' >> Prog.hs
runghc Prog.hs
rm Prog.hs
