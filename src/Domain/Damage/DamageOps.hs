module Domain.Damage.DamageOps (
  DamageOps(),
  dropUserItem,
  dropTargetItem,
  mkDamageOps
                               ) where


data DamageOps alg = DamageOps
  {
    dropUserItem :: alg ()
  , dropTargetItem :: alg ()
  }

mkDamageOps :: Monad m => m () -> m () -> DamageOps m
mkDamageOps u t = DamageOps u t
