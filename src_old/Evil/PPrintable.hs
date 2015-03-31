module Evil.PPrintable (
    PPrintable(..)
  ) where


-- pretty
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP


class PPrintable a where
    pprint :: a -> Doc


instance PPrintable ()    where pprint = const $ PP.text "()"
instance PPrintable Float where pprint = PP.float
