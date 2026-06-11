     *===============================================================*
     H/TITLE SCOPYEN01I - /COPY 'I' Specs for Endorsement programs
     *===============================================================*
     *  Created   : 07/26/99                                         *
     *  Programmer: C:113                                            *
     *  Project   : CM027                                            *
     *===============================================================*
     *  Revisions:                                                   *
     *                                                               *
     *                                                               *
     * ------------------------------------------------------------- *
    *  NOTE: Please add comments to the top of Revisions.           *
     * ------------------------------------------------------------- *
     *===============================================================*
     ** ZIP code breakdown

     D                 DS
     D  XXZIP                  1      9
     D  XXZIP1                 1      5
     D  XXZIP2                 6      9
     ** 7-digit date breakdown
     D                 DS
     D  DSDAT7                 1      7  0
     D  DSCENT                 1      1
     D  DSYY                   2      3
     D  DSMM                   4      5
     D  DSDD                   6      7
     ** Date & time breakdown
     D                 DS
     D  DSDTTM                 1     12  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     12  0
     ** Midterm data area
     D SVLDA           DS          1024
     D  LDEND#                 1      5  0
     D  LDENDA                 1      5
     D  LDMYRR                 6     14  0
     D  LDEEDT                15     22  0
     D  LDFYR                 23     25  0
     D  LDPYRL                26     26
     ** External data structure
     D PSDS          ESDS                  EXTNAME(SZQ1P)
