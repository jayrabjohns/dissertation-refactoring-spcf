module BoundedASPCF where

import BoundedSPCF

inj :: Term
inj = Lambda "f" undefined $ Product [(Catch $ Variable "f"), Product []]