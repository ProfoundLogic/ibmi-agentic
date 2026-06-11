     D SF              S              1    DIM(60) CTDATA PERRCD(1)             Screen Functions
     D SFD             S             35    DIM(60) ALT(SF)
     D AV_KEYS_DS      ds                  qualified
     D ARRAY                         81    dim(14)
     D KEYS                        1134    OVERLAY(AV_KEYS_DS)
     D FL              S             78    DIM(14)                              Function lines
     D FL1             S             78    DIM(14)                              Function lines
     D FL2             S             78    DIM(14)                              Function lines
     D FL3             S             78    DIM(14)                              Function lines
     D FL4             S             78    DIM(14)                              Function lines
     D FL5             S             78    DIM(14)                              Function lines
     D VF              S              1    DIM(24)                              Valid functions
     D VF1             S              1    DIM(24)                              Valid functions
     D VF2             S              1    DIM(24)                              Valid functions
     D VF3             S              1    DIM(24)                              Valid functions
     D VF4             S              1    DIM(24)                              Valid functions
     D VF5             S              1    DIM(24)                              Valid functions
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D*
     D ##SFD           DS          3500
     D*
     D MODEA           C                   CONST('-- Add --')
     D MODEC           C                   CONST('- Change -')
     D MODED           C                   CONST('-Display-')
     D PDSNAM          C                   CONST('StoneRiver')
