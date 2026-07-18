#if MIN_VERSION_ghc(9,0,0)
#define UNIQ_FM UniqFM Name [Fuse]
#define GET_NAME getName
#define FMAP_SND fmap snd $
#else
#define UNIQ_FM UniqFM [Fuse]
#define GET_NAME getName
#define FMAP_SND
#endif

#if MIN_VERSION_ghc(9,2,0)
#define ALT_CONSTR(x,y,z) Alt (x) y z
#else
#define ALT_CONSTR(x,y,z) (x, y, z)
#endif
