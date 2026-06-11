      /if defined(CONVERT)
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D FL1             S             78    DIM(14)                              Function lines
     D FL2             S             78    DIM(14)                              Function lines
     D FL3             S             78    DIM(14)                              Function lines
     D FL4             S             78    DIM(14)                              Function lines
     D FL5             S             78    DIM(14)                              Function lines
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D MODEA           C                   CONST('-- Add --')
     D MODEC           C                   CONST('- Change -')
     D MODED           C                   CONST('-Display-')
     D pdsnam          c                   const('Stoneriver, Inc.')
     DbrandName        s             80    inz
     D*
     D/copy SPrRtvBrnd
      /Endif
