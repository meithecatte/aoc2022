#!/bin/bash
sed 's/: / = /g' > Prog.hs
echo 'root :: Rational' >> Prog.hs
echo 'main = print root' >> Prog.hs
runghc Prog.hs
rm Prog.hs
