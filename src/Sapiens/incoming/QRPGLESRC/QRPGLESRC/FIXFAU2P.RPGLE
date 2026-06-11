     HCopyright('(c) 2021 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*caller)
     H Bnddir('STBNDDIR')
     H Bnddir('FTBNDDIR')
     H*****************************************************************
     H*  FIXFAU2P - Add missing FAU2P records to EOB's                *
     H****************************************************************f*
     H*                                                               *
     H*  Called from: Command line                                    *
     H*  Calls to   : None                                            *
     H*                                                               *
     H*****************************************************************
     H*  Created on 04/30/2021 by C:79 for Support PHW95736           *
     H*****************************************************************
     H*  Revisions:                                                   *
     H*                                                               *
     H*****************************************************************
     H*  Indicators used:                                             *
     H*                                                               *
     H*****************************************************************
     F*
     F* Transfer files
     F*
     FFAU1P     IF   E             DISK
     FFAU2L1    UF   E           K DISK
     FFAEOL31   IF   E           K DISK
     FFAEHL     IF   E           K DISK
     FFAEUL     IF   E           K DISK
     FFAEDL     IF   E           K DISK
     FFAEPL     IF   E           K DISK
     F*
     FFTBSL     IF   E           K DISK
     F*
     F* Detail files
     F*
     FFDCAP     O    E             DISK
     FFDCALF    IF   E           K DISK
     F                                     RENAME(PCA01:PCA02)
     FFDDAL2    IF A E           K DISK
     FFDDEP     O    E             DISK
     FFDDEL     IF   E           K DISK
     F                                     RENAME(PDE01:PDE02)
     FFDDHL     IF A E           K DISK
     FFDDPL     IF A E           K DISK
     FFDDUL     IF A E           K DISK
     FFDDDL     IF A E           K DISK
     F*
     F* Miscellaneous files
     F*
     FSTSTL     IF   E           K DISK
     F*
     FFMPRL     IF   E           K DISK    USROPN
     FFMSVL1    IF   E           K DISK    USROPN
     FFMRCL     IF   E           K DISK
     F*  Fee Schedule Base Control File
     FSSYLF     IF   E           K DISK
     F                                     IGNORE(SSY01)
     F                                     IGNORE(SPC01)
     F                                     IGNORE(SWC01)
     F                                     IGNORE(SRC01)
     F                                     IGNORE(SDR01)
     F                                     IGNORE(SBC01)
     F                                     IGNORE(SCS01)
     F                                     IGNORE(SGL01)
     F                                     IGNORE(SP201)
     FSMIEP     IF   E           K DISK
     f*
     ffmaml2    if   e           k Disk
     f*
     ffmdfl1    if   e           k Disk
     f*
     ffmdml     if   e           k Disk
     f*
     fftmel2tx  if   e           k Disk
     f*
     fftrvl3    if   e           k Disk
     f*
     fftlbl2    if   e           k Disk
     d pgmStr          pr
     d rtvSettings     pr                  Extpgm('SA016R')
     d  @@Cchg                        1
     d  @@Cifs                        1
     d  @@Csts                        1
     d  @@Cdny                        1
     d  @@Ccvfl                       1
     d  @@Cgrp                        1
     d  @@Cemp                        1
     d  @@Eifs                        1
     d  @@Eagt                        1
     d  @@Eemp                        1
     d  @@Fifs                        1
     d  @@Ftyp                        1
     d  @@Udup                        1
     d  @@Udrc                        1
     d  @@Urel                        1
     d  @@Upcl                        1
     d  @@Updn                        1
     d  @@Udpn                        1
     d  @@ClDays                      5  0
     d  @@IncZro                      1
     d  @@Crcd                        1
     d  @@CrDth                       1
     d  @@RtCd                        1
     d  @@Cchg         s              1
     d  @@Cifs         s              1
     d  @@Csts         s              1
     d  @@Cdny         s              1
     d  @@Ccvfl        s              1
     d  @@Cgrp         s              1
     d  @@Cemp         s              1
     d  @@Eifs         s              1
     d  @@Eagt         s              1
     d  @@Eemp         s              1
     d  @@Fifs         s              1
     d  @@Ftyp         s              1
     d  @@Udup         s              1
     d  @@Udrc         s              1
     d  @@Urel         s              1
     d  @@Upcl         s              1
     d  @@Updn         s              1
     d  @@Udpn         s              1
     d  @@ClDays       s              5  0
     d  @@IncZro       s              1
     d  @@Crcd         s              1
     d  @@CrDth        s              1
     d  @@RtCd         s              1
     d  isSettings     s              1
     d  xxRlin         s              1
     D CLM             S              1    DIM(20)
     D QCA             S             56    DIM(4) CTDATA PERRCD(1)
     D* Program constants.
     D DIGITS          C                   CONST('0123456789')
     D XXDESC          C                   CONST('All incl room and -
     D                                     board')
     D LO              c                   'abcdefghijklmnopqrstuvwxyz'
     D UP              c                   'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
     d xxFound         s              1
     D*================================================================
     D*  User defined data structures
     D*================================================================
     D                 DS
     D  DSDTTM                 1     14  0
     D  DSTIME                 1      6  0
     D  DSDATE                 7     14  0
     D                 DS
     D  KKCMSV                 1     11
     D  KKRVCD                 1      3    INZ('000')
     D  KKHCPC                 4      8
     D  KKUNTS                 9     11    INZ('000')
     D*
     D                 DS
     D  XXDXR                  1      4
     D  DX1                    1      1
     D  DX2                    2      2
     D  DX3                    3      3
     D  DX4                    4      4
     D*
     D*
     D                 DS
     D  VALUE                  1     11
     D  SIGN                   1      1
     D  AMOUNT                 2     11  2
     D                 DS
     D  CHRG1                  1     11
     D  CHRG2                  2     11  2
     D                 DS
     D  MRA1                   1     11
     D  MRA2                   2     11  2
     D                 DS
     D  NDIS1                  1     11
     D  NDIS2                  2     11  2
     D                 DS
     D  PAID1                  1     11
     D  PAID2                  2     11  2
     D                 DS
     D  QQCMD                  1     56
     D  QQST                  40     41
     D                 DS
     D  K1CMSV                 1     11
     D  K1RVCD                 1      3    INZ('000')
     D  K1RATE                 4      8
     D  K1UNIT                 9     11    INZ('000')
     D*
     D XXEDS           DS
     D  @@EDS1                 1    256
     D  @@EDS2               257    280
     D*
     D PSDS          ESDS                  EXTNAME(SZQ1P)
     D*
     I*
     I*================================================================
     I*  /COPY required constants
     I*================================================================
     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     DATCHK        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      *BLANKS       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
     C*
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI
     C                   PARM      *BLANKS       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTCYM2        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*YYMD'       SRFFMT
     C                   PARM      '*CYMD'       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*
     C     DTMDYY        PLIST
     C                   PARM                    SRDTI
     C                   PARM      '*CYMD'       SRFFMT
     C                   PARM      *BLANKS       SRTFMT
     C                   PARM                    SRDTO
     C                   PARM                    SRERR
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     CAKY          KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKDSEQ
     C*
     C     CAKY2         KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C*
     C     CAPKY1        KLIST
     C                   KFLD                    U1CO#
     C                   KFLD                    U1SUB#
     C                   KFLD                    U1FND
     C                   KFLD                    U1FYR
     C                   KFLD                    U1CASE
     C                   KFLD                    KKEOB#
     C                   KFLD                    KKDSEQ
     C*
     C*
     C     DEKY1         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C*
     C     DEKY2         KLIST
     C                   KFLD                    CACO#
     C                   KFLD                    CASUB#
     C                   KFLD                    CAFND
     C                   KFLD                    CAFYR
     C                   KFLD                    CACASE
     C                   KFLD                    CAEOB#
     C                   KFLD                    CADSEQ
     C                   KFLD                    KKSEQ#
     C*
     C     STKY          KLIST
     C                   KFLD                    KKST             10
     C*
     C     PRKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     PRKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKPRCD
     C*
     C     SVKY          KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    K1CMSV
     C                   KFLD                    KKFDAT
     C*
     C     SVKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    K1CMSV
     C*
     C*
     C     RCKY1         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKPRCD
     C                   KFLD                    KKFDAT
     C*
     C     RCKY2         KLIST
     C                   KFLD                    EOCO#
     C                   KFLD                    KKZIPA
     C                   KFLD                    KKPRCD
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    CACO#         KKCO#
     C     *LIKE         DEFINE    CAEOB#        KKEOB#
     C     *LIKE         DEFINE    CADSEQ        KKDSEQ
     C     *LIKE         DEFINE    CADSEQ        XXDSEQ
     C     *LIKE         DEFINE    CALIN#        KKLIN#
     C     *LIKE         DEFINE    DESEQ#        KKSEQ#
     C     *LIKE         DEFINE    DESEQ#        HDSEQ#
     C     *LIKE         DEFINE    BSTYPE        KKTYPE
     C     *LIKE         DEFINE    BSCODE        KKCODE
     C     *LIKE         DEFINE    CALIN#        XXLIN#
     C     *LIKE         DEFINE    DHPRCD        KKPRCD
     C     *LIKE         DEFINE    CAFDAT        KKFDAT
     C     *LIKE         DEFINE    DENOT1        XXEDS1
     C     *LIKE         DEFINE    DENOT1        XXEDS2
     C     *LIKE         DEFINE    DENOT1        XXEDS3
     C     *LIKE         DEFINE    DENOT1        XXEDS4
     C     *LIKE         DEFINE    CATAX         XXTAX
     C     *like         Define    STmfs         xxMfs
     C     *like         Define    RCZIPA        kkZipA
     C
       // Call program start procedure
       callp pgmStr();

       // EXECUTE MAIN SUBROUTINE

       Exsr MainSr;

     C*================================================================
     C* DLTOVR - DELETE OVERRIDES
     C*================================================================
     C     DLTOVR        BEGSR
     C*
     C* Delete overrides,
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(3)
     C                   PARM                    LENGTH           15 5
     C*
     C* Delete overrides,
     C*
     C                   Z-ADD     56            LENGTH
     C                   CALL      'QCMDEXC'
     C                   PARM                    QCA(4)
     C                   PARM                    LENGTH           15 5
     C*
     C                   CLOSE     FMPRL                                99
     C                   CLOSE     FMSVL1                               99
     C*
     C                   ENDSR
       //================================================================
       // MAINSR - MAIN SUBROUTINE
       //================================================================
       Begsr Mainsr;

       // Read through EOB's (FAU1P) that have missing detail records
       Read pu101;
       Dow not %Eof(fau1p);

          Chain (U1co#:U1eob#) peo01;
          If %Found(faeol31);
             Exsr Ovrst;

             // Get the correct header file data for use in loading ICD10 pointers
             Select;
             When EObTyp = 'N';
                Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) peh01;
             When EObTyp = 'H';
                Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) peu01;
             When EObTyp = 'D';
                Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) ped01;
             When EObTyp = 'X';
                Chain (EOco#:EOsub#:EOfnd:EOfyr:EOcase:EOeob#) pep01;

             Endsl;

             // Read through all missing detail records and write to the detail files
             Setll (U1Trid) pu201;
             Reade (U1Trid) pu201;
             Dow not %Eof(fau2l1);
                Exsr WrtDtl;
                U2prFl = 'Y';
                U2eob# = CAeob#;
                U2dSeq = CAdSeq;
                Update pu201;
                Reade (U1Trid) pu201;
             Enddo;
          Endif;
          Read pu101;

       Enddo;

       *Inlr = *on;
       Return;

       Endsr;
     C*================================================================
     C* WRTDE1 - Write FDDEP record
     C*================================================================
     C     WRTDE1        BEGSR
     C*
     C                   CLEAR                   PDE01
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   ADD       1             HDSEQ#
     C*
     C                   MOVEL(P)  XXEOB         DEEOB
     C     *IN72         IFEQ      *ON
     C                   MOVEL(P)  XXEDS1        DENOT1
     C                   MOVEL(P)  XXEDS2        DENOT2
     C                   MOVEL(P)  XXEDS3        DENOT3
     C                   MOVEL(P)  XXEDS4        DENOT4
     C                   ELSE
     C                   MOVEL(P)  XXEODA        DENOT1
     C                   MOVEL(P)  XXEODB        DENOT2
     C     1             SUBST     DENOT2:1      XXPOS1            1
     C     XXPOS1        IFEQ      *BLANK
     C     69            SUBST(P)  DENOT2:2      DENOT2
     C                   ENDIF
     C                   ENDIF                                                  *IN72 = *ON
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CADT          DEDT
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CAITIM        DEITIM
     C                   Z-ADD     CAIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C*
     C                   ENDSR
     C*================================================================
     C* WRTCAP - Write FDCAP record
     C*================================================================
     C     WRTCAP        BEGSR
     C*
     C     U2EOB#        IFEQ      *ZEROS
     C     U2DSEQ        ANDEQ     *ZEROS
     C*
     C                   Z-ADD     *HIVAL        KKDSEQ
     C     CAKY          SETGT     PCA02
     C     CAKY2         READPE    PCA02                                  99
     C     *IN99         IFEQ      *OFF
     C     CADSEQ        ADD       1             XXDSEQ
     C                   ELSE
     C                   Z-ADD     1             XXDSEQ
     C                   ENDIF
     C*
     C                   CLEAR                   PCA01
     C                   Z-ADD     EOCO#         CACO#
     C                   Z-ADD     EOFND         CAFND
     C                   Z-ADD     EOFYR         CAFYR
     C                   Z-ADD     EOCASE        CACASE
     C                   MOVEL     EOCSCD        CACSCD
     C*
     C                   Z-ADD     U2SVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         CAFDAT
     C*
     C                   Z-ADD     CAFDAT        CATDAT
     C                   Z-ADD     EOTRDT        CARDAT
     C                   Z-ADD     EOFEIN        CAFEIN
     C                   Z-ADD     EOMOD         CAMOD
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C                   MOVEL     '1'           CAPRTY
     C     EOBTYP        WHENEQ    'H'
     C                   MOVEL     '4'           CAPRTY
     C     EOBTYP        WHENEQ    'D'
     C                   MOVEL     'D'           CAPRTY
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL     'X'           CAPRTY
     C                   ENDSL
     C*
     C     U2UNTS        IFNE      *ZEROS
     C                   Z-ADD     U2UNTS        CAUNTS
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      CAunts = U2unts/100
     C                   endif
     C                   ELSE
     C                   Z-ADD     1             CAUNTS
     C                   ENDIF
     C*
     C                   Z-ADD     XXDSEQ        CADSEQ
     C     EOBLST        IFEQ      'P'
     C                   Z-ADD     0             CACHK#
     C                   ELSE
     C                   Z-ADD     EOCHK#        CACHK#
     C                   ENDIF
     C                   Z-ADD     EOEOB#        CAEOB#
     C                   Z-ADD     EORCN         CARCN
     C                   MOVEL     EOSS#         CASS#
     C                   Z-ADD     EOACDT        CAACDT
     C                   MOVEL     'A'           CASTAT
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C                   MOVEL(P)  'D'           CARCFL
     C     EOBTYP        WHENEQ    'H'
     C                   MOVEL(P)  'S'           CARCFL
     C     EOBTYP        WHENEQ    'N'
     C                   MOVEL(P)  'C'           CARCFL
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL(P)  'X'           CARCFL
     C                   ENDSL
       //
       // Set CArCfl and CAsrc based on State flag
       //
      /Free
                         SetCodeType();
      /End-Free
     C*
     C                   MOVEL(P)  Q1USER        CAUSER
     C                   MOVEL(P)  Q1PGM         CAPGM
     C                   TIME                    DSDTTM
     C                   Z-ADD     DSTIME        CATIME
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         CADT
     C                   MOVEL(P)  Q1USER        CAIUSR
     C                   MOVEL(P)  Q1PGM         CAIPGM
     C                   Z-ADD     CADT          CAIDT
     C                   Z-ADD     CATIME        CAITIM
     C*
     C                   Z-ADD     EOSUB#        CASUB#
     C                   MOVE      EOZIPA        CAZIPA
     C                   MOVEL(P)  EOAST         CAAST
     C*
     C                   MOVE      U2CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CACHRG
     C                   ELSE
     C                   Z-ADD     AMOUNT        CACHRG
     C                   ENDIF
     C*
     C                   MOVE      U2MRA         VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CAMRA
     C                   ELSE
     C                   Z-ADD     AMOUNT        CAMRA
     C                   ENDIF
     C*
     C     U2OTH2        IFNE      *BLANKS
     C                   MOVE      U2OTH2        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CAOTH2
     C                   ELSE
     C                   Z-ADD     AMOUNT        CAOTH2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      U2NDIS        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CANDIS
     C                   ELSE
     C                   Z-ADD     AMOUNT        CANDIS
     C                   ENDIF
     C*
     C                   MOVE      U2PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        CAPAID
     C                   ELSE
     C                   Z-ADD     AMOUNT        CAPAID
     C                   ENDIF
     C*
     C                   Z-ADD     U2LIN#        CALIN#
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'N'
     C     U2CODE        IFEQ      *BLANKS
     C                   MOVEL(P)  '96370'       CACODE
     C                   MOVEL(P)  '96370'       CACPT4
     C                   MOVEL(P)  '96370'       CABPRC
     C                   ELSE
     C                   MOVEL(P)  U2CODE        CACODE
     C                   MOVEL(P)  U2CODE        CACPT4
     C                   MOVEL(P)  U2CODE        CABPRC
     C                   ENDIF
     C     EOBTYP        WHENEQ    'D'
     C                   MOVEL(P)  U2CODE        CACODE
     C                   MOVEL(P)  U2CODE        CACPT4
     C                   MOVEL(P)  U2CODE        CABPRC
     C     EOBTYP        WHENEQ    'X'
     C                   MOVEL(P)  U2NDC         CACODE
     C     EOBTYP        WHENEQ    'H'
     C                   RESET                   KKCMSV
     C     DIGITS        CHECK     U2RVCD        N                 3 0    99
     C     *IN99         IFEQ      *ON
     C     U2CODE        IFEQ      *BLANKS
     C                   MOVEL     U2RVCD        KKHCPC
     C                   ELSE
     C                   MOVEL(P)  U2CODE        KKHCPC
     C                   ENDIF
     C                   ELSE
     C                   MOVE      U2RVCD        KKRVCD
     C                   MOVEL(P)  U2CODE        KKHCPC
     C                   ENDIF
     C                   MOVE      KKRVCD        CASVCD
     C                   MOVEL     KKCMSV        CACODE
     C                   ENDSL
     C                   WRITE     PCA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDAP - Write FDDAP record
     C*================================================================
     C     WRTDAP        BEGSR
     C*
     C                   MOVE      U1CHRG        CHRG1
     C                   MOVE      U1MRA         MRA1
     C                   MOVE      U1NDIS        NDIS1
     C                   MOVE      U1PAID        PAID1
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDA01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDA01
     C                   Z-ADD     CACO#         DACO#
     C                   Z-ADD     CASUB#        DASUB#
     C                   Z-ADD     CAFND         DAFND
     C                   Z-ADD     CAFYR         DAFYR
     C                   Z-ADD     CACASE        DACASE
     C                   MOVEL     CASS#         DASS#
     C                   Z-ADD     CAACDT        DAACDT
     C                   Z-ADD     CAEOB#        DAEOB#
     C                   Z-ADD     EORCN         DARCN#
     C                   Z-ADD     CADSEQ        DADSEQ
     C                   Z-ADD     CARDAT        DATRDT
     C*
     C                   MOVE      U2CHRG        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DACHRG
     C                   ELSE
     C                   Z-ADD     AMOUNT        DACHRG
     C                   ENDIF
     C*
     C                   MOVE      U2MRA         VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DAMRA
     C                   ELSE
     C                   Z-ADD     AMOUNT        DAMRA
     C                   ENDIF
     C*
     C     U2OTH2        IFNE      *BLANKS
     C                   MOVE      U2OTH2        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DAOTH2
     C                   ELSE
     C                   Z-ADD     AMOUNT        DAOTH2
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      U2NDIS        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DANDIS
     C                   ELSE
     C                   Z-ADD     AMOUNT        DANDIS
     C                   ENDIF
     C*
     C                   MOVE      U2PAID        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DAPAID
     C                   ELSE
     C                   Z-ADD     AMOUNT        DAPAID
     C                   ENDIF
      /Free
          If u2tax = *blanks;
             u2tax = *zeros;
          Endif;
          If U2tax <> *blanks;
          Endif;
      /End-Free
     C*
     C*
     C                   MOVEL     'A'           DASTAT
     C                   MOVEL(P)  CAUSER        DAUSER
     C                   MOVEL(P)  CAPGM         DAPGM
     C                   Z-ADD     CATIME        DATIME
     C                   Z-ADD     CADT          DADT
     C                   MOVEL(P)  CAUSER        DAIUSR
     C                   MOVEL(P)  CAPGM         DAIPGM
     C                   Z-ADD     CAITIM        DAITIM
     C                   Z-ADD     CAIDT         DAIDT
     C*
     C                   WRITE     PDA01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDDP - Write FDDDP record
     C*================================================================
     C     WRTDDP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDD01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDD01
     C                   Z-ADD     CACO#         DDCO#
     C                   Z-ADD     CASUB#        DDSUB#
     C                   Z-ADD     CAFND         DDFND
     C                   Z-ADD     CAFYR         DDFYR
     C                   Z-ADD     CACASE        DDCASE
     C                   MOVEL     CASS#         DDSS#
     C                   Z-ADD     CAACDT        DDACDT
     C                   Z-ADD     CAEOB#        DDEOB#
     C                   Z-ADD     CADSEQ        DDDSEQ
     C*
     C     U2UNTS        IFEQ      *ZEROS
     C                   Z-ADD     1             DDUNTS
     C                   ELSE
     C                   Z-ADD     U2UNTS        DDUNTS
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      DDunts = U2unts/100
     C                   endif
     C                   ENDIF
     C*
     C                   MOVEL     U2CODE        DDPRCD
     C                   MOVEL     U2CDSC        DDPRDS
     C                   MOVEL     U2CODE        DDBPRC
     C                   MOVEL     U2CDSC        DDBPRD
     C                   MOVEL     U2POS         DDPOS
     C*
     C     DDPOS         IFEQ      *BLANKS
     C     DDPOS         OREQ      '00'
     C                   MOVEL     '11 '         DDPOS
     C                   ENDIF
     C*
     C                   MOVE      'C'           DDRCFL
     C*
     C                   MOVE      U2DXR         XXDXR
     C*
     C     DX1           IFEQ      '1'
     C     DX1           OREQ      'A'
     C     DX2           OREQ      '1'
     C     DX2           OREQ      'A'
     C     DX3           OREQ      '1'
     C     DX3           OREQ      'A'
     C     DX4           OREQ      '1'
     C     DX4           OREQ      'A'
     C                   MOVEL     'X'           DDDXR1
     C                   ENDIF
     C*
     C     DX1           IFEQ      '2'
     C     DX1           OREQ      'B'
     C     DX2           OREQ      '2'
     C     DX2           OREQ      'B'
     C     DX3           OREQ      '2'
     C     DX3           OREQ      'B'
     C     DX4           OREQ      '2'
     C     DX4           OREQ      'B'
     C                   MOVEL     'X'           DDDXR2
     C                   ENDIF
     C*
     C     DX1           IFEQ      '3'
     C     DX1           OREQ      'C'
     C     DX2           OREQ      '3'
     C     DX2           OREQ      'C'
     C     DX3           OREQ      '3'
     C     DX3           OREQ      'C'
     C     DX4           OREQ      '3'
     C     DX4           OREQ      'C'
     C                   MOVEL     'X'           DDDXR3
     C                   ENDIF
     C*
     C     DX1           IFEQ      '4'
     C     DX1           OREQ      'D'
     C     DX2           OREQ      '4'
     C     DX2           OREQ      'D'
     C     DX3           OREQ      '4'
     C     DX3           OREQ      'D'
     C     DX4           OREQ      '4'
     C     DX4           OREQ      'D'
     C                   MOVEL     'X'           DDDXR4
     C                   ENDIF
     C*
     C                   SELECT
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA1        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR1
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA2        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR2
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA3        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR3
     C     DDDXR1        WHENEQ    *BLANKS
     C     DDDXR2        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     DDDXR3        ANDEQ     *BLANKS
     C     EDDIA4        ANDNE     *BLANKS
     C                   MOVEL     'X'           DDDXR4
     C                   ENDSL
     C                   MOVEL(P)  U2TOO#        DDPART
     C                   MOVEL(P)  U2BPRC        DDBPRC
     C                   If        U2mod = *blanks
     C                   MOVEL(P)  U2BMD1        DDMOD1
     C                   Else
     C                   Eval      DDmod1 = U2mod
     C*
     C                   Endif
     C*
     C                   MOVEL     'A'           DDSTAT
     C                   MOVEL(P)  CAUSER        DDUSER
     C                   MOVEL(P)  CAPGM         DDPGM
     C                   Z-ADD     CATIME        DDTIME
     C                   Z-ADD     CADT          DDDT
     C                   MOVEL(P)  CAUSER        DDIUSR
     C                   MOVEL(P)  CAPGM         DDIPGM
     C                   Z-ADD     CAITIM        DDITIM
     C                   Z-ADD     CAIDT         DDIDT
     C*
     C                   WRITE     PDD01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDEP - Write FDDEP record
     C*================================================================
     C     WRTDEP        BEGSR
     C*
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY2         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             HDSEQ#
     C                   ELSE
     C                   Z-ADD     1             HDSEQ#
     C                   ENDIF
     C*
     C* This code checks for EOB Codes/Notes in the expanded fields
     C*
     C                   SETOFF                                       71
     C     U2EOB1        IFNE      *BLANKS
     C     U2EOD1        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB1        XXEOB             3
      /Free
                         If %Subst(U2eob1:4:1) <> ' ' and
                           %Subst(U2eob1:2:1) <> ' ';
                            xxEob = %Subst(U2eob1:2:3);
                         Endif;
      // We are not using the exanded fields U2eDs1-4
                         If U2Eod1 <> *blanks;
                            *in72 = *off;
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD1:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD1:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD1:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C*
     C                   ENDIF
     C*
     C     U2EOB2        IFNE      *BLANKS
     C     U2EOD2        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB2        XXEOB
      /Free
                         If %Subst(U2eob2:4:1) <> ' ' and
                           %Subst(U2eob2:2:1) <> ' ';
                            xxEob = %Subst(U2eob2:2:3);
                         Endif;
      // We are not using the exanded fields U2eDs1-4
                         If U2Eod2 <> *blanks;
                            *in72 = *off;
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD2:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD2:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD2:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C*
     C                   ENDIF
     C*
     C     U2EOB3        IFNE      *BLANKS
     C     U2EOD3        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB3        XXEOB
      /Free
                         If %Subst(U2eob3:4:1) <> ' ' and
                           %Subst(U2eob3:2:1) <> ' ';
                            xxEob = %Subst(U2eob3:2:3);
                         Endif;
      // We are not using the exanded fields U2eDs1-4
                         If U2Eod3 <> *blanks;
                            *in72 = *off;
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD3:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD3:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD3:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C                   ENDIF
     C*
     C     U2EOB4        IFNE      *BLANKS
     C     U2EOD4        ORNE      *BLANKS
     C                   MOVEL(P)  U2EOB4        XXEOB
      /Free
                         If %Subst(U2eob4:4:1) <> ' ' and
                           %Subst(U2eob4:2:1) <> ' ';
                            xxEob = %Subst(U2eob4:2:3);
                         Endif;
      // We are not using the exanded fields U2eDs1-4
                         If U2Eod4 <> *blanks;
                            *in72 = *off;
                         Endif;
      /End-free
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     U2EOD4:N1     XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  U2EOD4:1      XXEODA           70
     C     N1            ADD       1             P1                3 0
     C     140           SUB       N1            P2                3 0
     C*
     C     P2            SUBST(P)  U2EOD4:P1     XXEODB           70
     C*
     C                   EXSR      WRTDE1
     C                   SETON                                        71
     C                   ENDIF
     C*
     C*
     C* Use the old fields if the new fields are not populated
     C*
     C     *IN71         IFEQ      *OFF
     C*
     C     U2EOB         IFNE      *BLANKS
     C     U2EOBD        ORNE      *BLANKS
     C                   CLEAR                   PDE01
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   ADD       1             HDSEQ#
     C*
     C     U2EOB         IFNE      *BLANKS
     C     U2EOB         CAT(P)    '-':1         DENOT1
     C                   CAT       U2EOBD:1      DENOT1
     C                   ELSE
     C                   MOVEL(P)  U2EOBD        DENOT1
     C                   ENDIF
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CADT          DEDT
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CAITIM        DEITIM
     C                   Z-ADD     CAIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C                   ENDIF
     C                   ENDIF                                                  IN71=OFF
     C*
     C     U2REAS        IFNE      *BLANKS
     C                   CLEAR                   PDE01
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   MOVEL(P)  U2REAS        DENOT1
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CADT          DEDT
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CAITIM        DEITIM
     C                   Z-ADD     CAIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDHP - Write FDDHP record
     C*================================================================
     C     WRTDHP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDH01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDH01
     C                   Z-ADD     CACO#         DHCO#
     C                   Z-ADD     CASUB#        DHSUB#
     C                   Z-ADD     CAFND         DHFND
     C                   Z-ADD     CAFYR         DHFYR
     C                   Z-ADD     CACASE        DHCASE
     C                   MOVEL     CASS#         DHSS#
     C                   Z-ADD     CAACDT        DHACDT
     C                   Z-ADD     CAEOB#        DHEOB#
     C                   Z-ADD     CADSEQ        DHDSEQ
     C*
     C     U2CODE        IFNE      *BLANKS
     C                   MOVEL     U2CODE        DHPRCD
     C                   MOVEL     U2CDSC        DHPRDS
     C                   MOVEL     U2CODE        DHBPRC
     C                   MOVEL     U2CDSC        DHBPRD
     C     DHPRDS        IFEQ      *BLANKS
     C*
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   MOVEL     DHPRCD        KKPRCD
     C*
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PRDSC1        DHPRDS
     C                   MOVEL     PRDSC1        DHBPRD
     C*
     C                   ELSE
     C* If not found in above files and state uses R&C file then find
     C* description from FMRCP
     C*
     C*
     C*  Get flag to determine if we use FMRCP file for Adj State
     C*  when the HCPCS code is not found in FMPRP,FMSVP files
     C*
     C                   MOVEL     'N'           ##RC              1
     C                   MOVEL(P)  EOAST         KKST
     C*
     C     STKY          CHAIN     PST01                              99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     STBS02        ##RC
     C                   ENDIF
     C*
     C     ##RC          IFNE      'N'
     C                   MOVEL     CAZIPA        KKZIPA
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        DHPRDS
     C                   MOVEL(P)  RCDSC1        DHBPRD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDIF
     C                   ENDIF
     C*
     C                   MOVE      U2POS         DHPOS
     C*
     C     DHPOS         IFEQ      *BLANKS
     C     DHPOS         OREQ      '00'
     C                   MOVEL     ' 11'         DHPOS
     C                   ENDIF
     C*
     C                   MOVEL     U2MOD         DHMOD1
     C                   MOVE      'C'           DHRCFL
     C*
     C     U2UNTS        IFEQ      *ZEROS
     C                   Z-ADD     1             DHUNTS
     C                   ELSE
     C                   Z-ADD     U2UNTS        DHUNTS
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      DHunts = U2unts/100
     C                   endif
     C                   ENDIF
     C*
     C                   MOVE      U2DXR         XXDXR
     C*
     C     DX1           IFEQ      '1'
     C     DX1           OREQ      'A'
     C     DX2           OREQ      '1'
     C     DX2           OREQ      'A'
     C     DX3           OREQ      '1'
     C     DX3           OREQ      'A'
     C     DX4           OREQ      '1'
     C     DX4           OREQ      'A'
     C                   MOVEL     'X'           DHDXR1
     C                   ENDIF
     C*
     C     DX1           IFEQ      '2'
     C     DX1           OREQ      'B'
     C     DX2           OREQ      '2'
     C     DX2           OREQ      'B'
     C     DX3           OREQ      '2'
     C     DX3           OREQ      'B'
     C     DX4           OREQ      '2'
     C     DX4           OREQ      'B'
     C                   MOVEL     'X'           DHDXR2
     C                   ENDIF
     C*
     C     DX1           IFEQ      '3'
     C     DX1           OREQ      'C'
     C     DX2           OREQ      '3'
     C     DX2           OREQ      'C'
     C     DX3           OREQ      '3'
     C     DX3           OREQ      'C'
     C     DX4           OREQ      '3'
     C     DX4           OREQ      'C'
     C                   MOVEL     'X'           DHDXR3
     C                   ENDIF
     C*
     C     DX1           IFEQ      '4'
     C     DX1           OREQ      'D'
     C     DX2           OREQ      '4'
     C     DX2           OREQ      'D'
     C     DX3           OREQ      '4'
     C     DX3           OREQ      'D'
     C     DX4           OREQ      '4'
     C     DX4           OREQ      'D'
     C                   MOVEL     'X'           DHDXR4
     C                   ENDIF
     C*
     C                   SELECT
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR4        ANDEQ     *BLANKS
     C     EHDIA1        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR1
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR4        ANDEQ     *BLANKS
     C     EHDIA2        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR2
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR4        ANDEQ     *BLANKS
     C     EHDIA3        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR3
     C     DHDXR1        WHENEQ    *BLANKS
     C     DHDXR2        ANDEQ     *BLANKS
     C     DHDXR3        ANDEQ     *BLANKS
     C     DHDXR4        ANDEQ     *BLANKS
     C     EHDIA4        ANDNE     *BLANKS
     C                   MOVEL     'X'           DHDXR4
     C                   ENDSL
     C                   MOVEL(P)  U2MOD2        DHMOD2
     C                   MOVEL(P)  U2MOD3        DHMOD3
     C                   MOVEL(P)  U2MOD4        DHMOD4
     C                   MOVEL(P)  U2BLFC        DHBLFC
     C                   MOVEL(P)  U2DUCD        DHDUCD
     C                   MOVEL(P)  U2BPRC        DHBPRC
     C                   If        DHmod1 = *blanks
     C                   MOVEL(P)  U2BMD1        DHMOD1
     C                   Endif
     C*
     C                   Eval      DHbMd1 = U2bMd1
     C*
     C                   MOVEL     'A'           DHSTAT
     C                   MOVEL(P)  CAUSER        DHUSER
     C                   MOVEL(P)  CAPGM         DHPGM
     C                   Z-ADD     CATIME        DHTIME
     C                   Z-ADD     CADT          DHDT
     C                   MOVEL(P)  CAUSER        DHIUSR
     C                   MOVEL(P)  CAPGM         DHIPGM
     C                   Z-ADD     CAITIM        DHITIM
     C                   Z-ADD     CAIDT         DHIDT
      /Free
          DHnpir = U2rnpi;
          DHrnlc = U2rnlc;
      /End-Free
     C*
     C                   WRITE     PDH01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDPP - Write FDDPP record
     C*================================================================
     C     WRTDPP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDP01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDP01
     C                   Z-ADD     CACO#         DPCO#
     C                   Z-ADD     CASUB#        DPSUB#
     C                   Z-ADD     CAFND         DPFND
     C                   Z-ADD     CAFYR         DPFYR
     C                   Z-ADD     CACASE        DPCASE
     C                   MOVEL     CASS#         DPSS#
     C                   Z-ADD     CAACDT        DPACDT
     C                   Z-ADD     CAEOB#        DPEOB#
     C                   Z-ADD     CADSEQ        DPDSEQ
     C                   MOVEL(P)  U2NDC         DPNDC
     C                   MOVEL(P)  U2RX#         DPRX#
     C                   MOVEL(P)  DPNDC         DPBNDC
     C                   Z-ADD     U2UNTS        XXQTY             9 0
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      XXQTY = U2unts/100
     C                   endif
     C                   Z-ADD     XXQTY         DPRXMQ
     C                   MOVEL(P)  U2CDSC        DPDESC
     C*
     C                   Z-ADD     U2SVDT        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYM2
     C                   MOVEL     SRDTO         DPRXFD
     C*
     C     U2UNTS        IFEQ      *ZEROS
     C                   Z-ADD     1             DPUNTS
     C                   ELSE
     C                   Z-ADD     U2UNTS        DPUNTS
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      DPunts = U2unts/100
     C                   endif
     C                   ENDIF
     C                   MOVEL(P)  U2RXPT        DPRXPT
     C*
     C     EOBTYP        IFEQ      'X'
     C     DPRXPT        ANDEQ     'S'
     C                   MOVEL(P)  U2PRIN        DPPRIN
     C                   MOVEL(P)  U2CODE        DPHCPC
     C                   ENDIF
     C*
     C                   MOVEL(P)  U2NWRF        DPNWRF
     C                   MOVEL(P)  U2CERT        DPCERT
     C                   MOVEL(P)  U2LIC#        DPLIC#
     C                   MOVEL(P)  U2RXDY        DPRXDY
     C*
     C*
     C                   MOVE      U2DFEE        VALUE
     C     SIGN          IFEQ      '-'
     C                   Z-SUB     AMOUNT        DPDFEE
     C                   ELSE
     C                   Z-ADD     AMOUNT        DPDFEE
     C                   ENDIF
     C*
     C*
     C                   MOVEL(P)  U2RXDI        DPRXDI
     C                   MOVEL(P)  U2BCDT        DPBCDT
     C                   MOVEL(P)  U2BNDC        DPBNDC
     C*
     C                   MOVEL     'A'           DPSTAT
     C                   MOVEL(P)  CAUSER        DPUSER
     C                   MOVEL(P)  CAPGM         DPPGM
     C                   Z-ADD     CATIME        DPTIME
     C                   Z-ADD     CADT          DPDT
     C                   MOVEL(P)  CAUSER        DPIUSR
     C                   MOVEL(P)  CAPGM         DPIPGM
     C                   Z-ADD     CAITIM        DPITIM
     C                   Z-ADD     CAIDT         DPIDT
     C*
     C                   WRITE     PDP01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDTL - Write Detail Records
     C*================================================================
     C     WRTDTL        BEGSR
     C*
     C                   EXSR      CHKAM2
     C                   EXSR      WRTCAP                                       FDCAP
     C                   EXSR      WRTDAP                                       FDDAP
     C*
     C                   SELECT
     C     EOBTYP        WHENEQ    'D'
     C                   EXSR      WRTDDP                                       FADDP
     C     EOBTYP        WHENEQ    'H'
     C                   EXSR      WRTDUP                                       FADUP
     C     EOBTYP        WHENEQ    'N'
     C                   EXSR      WRTDHP                                       FADHP
     C     EOBTYP        WHENEQ    'X'
     C                   EXSR      WRTDPP                                       FADHP
     C                   ENDSL
     C     U2EDS1        IFNE      *Blanks
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS1        XXEDS
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB1        XXEOB             3
     C     ' '           CAT       XXEOB         XXEOB
     C                   EXSR      WRTEDS                                       FADEP
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS2        XXEDS
     C     ' '           CAT       XXEOB         XXEOB
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB2        XXEOB             3
     C     ' '           CAT       XXEOB         XXEOB
     C                   EXSR      WRTEDS                                       FADEP
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS3        XXEDS
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB3        XXEOB             3
     C     ' '           CAT       XXEOB         XXEOB
     C                   EXSR      WRTEDS                                       FADEP
     C                   CLEAR                   XXEDS
     C                   MOVEL(P)  U2EDS4        XXEDS
     C                   CLEAR                   XXEOB
     C                   MOVEL(P)  U2EOB4        XXEOB             3
     C     ' '           CAT       XXEOB         XXEOB
     C                   EXSR      WRTEDS                                       FADEP
     C*
     C                   ELSE                                                   C139 = *ON
     C                   EXSR      WRTDEP                                       FADEP
     C*
     C                   ENDIF                                                  C139 = *ON
     C*
     C                   ENDSR
     C*================================================================
     C* WRTDUP - Write FDDUP record
     C*================================================================
     C     WRTDUP        BEGSR
     C*
     C                   Z-ADD     CAEOB#        KKEOB#
     C                   Z-ADD     CADSEQ        KKDSEQ
     C     CAPKY1        CHAIN     PDU01                              99
     C     *IN99         IFEQ      *ON
     C*
     C                   CLEAR                   PDU01
     C                   Z-ADD     CACO#         DUCO#
     C                   Z-ADD     CASUB#        DUSUB#
     C                   Z-ADD     CAFND         DUFND
     C                   Z-ADD     CAFYR         DUFYR
     C                   Z-ADD     CACASE        DUCASE
     C                   MOVEL     CASS#         DUSS#
     C                   Z-ADD     CAACDT        DUACDT
     C                   Z-ADD     CAEOB#        DUEOB#
     C                   Z-ADD     CADSEQ        DUDSEQ
     C*
     C                   MOVEL     KKRVCD        DURVCD
     C                   MOVEL     KKHCPC        DURATE
     C*
     C                   MOVE      U2POS         DUPOS
     C*
     C     DUPOS         IFEQ      *BLANKS
     C     DUPOS         OREQ      '00'
     C                   MOVEL     ' 21'         DUPOS
     C                   ENDIF
     C*
     C                   Z-ADD     U2UNTS        DUUNTS
     C                   if        %xlate(lo:up:U1ntwk) = 'OPTUM'
     C                   eval      DUunts = U2unts/100
     C                   endif
     C                   MOVE      DURVCD        DUBPRC
     C                   MOVE      DURATE        DUBRAT
     C                   MOVEL(P)  U2CDSC        DUDESC
     C                   MOVEL(P)  DUDESC        DUBPRD
     C                   MOVEL(P)  U2DUCD        DUDUCD
     C                   MOVEL(P)  U2BPRC        DUBRAT
     C                   MOVEL(P)  U2BMD1        DUPMOD
     C*
     C                   MOVEL(P)  U2MOD         DUPMOD
     C                   MOVEL(P)  U2MOD2        DUMOD2
     C                   MOVEL(P)  U2PSCD        DUPSCD
     C                   MOVEL(P)  U2CDSC        DUSCDS
     C     DUDESC        IFEQ      *BLANKS
     C*
     C                   Z-ADD     CAFDAT        KKFDAT
     C                   MOVEL     DURVCD        K1RVCD
     C                   MOVEL     DURATE        K1RATE
     C                   MOVE      *ZEROS        K1UNIT
     C*
     C                   SELECT
     C*
     C     DURVCD        WHENNE    *ZEROS
     C     DURATE        ANDEQ     *BLANKS
     C*
     C     SVKY          SETGT     PSV01
     C     SVKY1         READPE    PSV01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     SVDESC        DUDESC
     C                   MOVEL     SVDESC        DUBPRD
     C                   ENDIF
     C*
     C     DURATE        WHENNE    *BLANKS
     C*
     C                   MOVEL     DURATE        KKPRCD
     C     PRKY          SETGT     PPR01
     C     PRKY1         READPE    PPR01                                  99
     C     *IN99         IFEQ      *OFF
     C                   MOVEL     PRDSC1        DUDESC
     C                   MOVEL     PRDSC1        DUBPRD
     C*
     C                   ELSE
     C* If not found in above files and state uses R&C file then find
     C* description from FMRCP
     C*
     C     ##RC          IFNE      'N'
     C*
     C                   MOVEL     CAZIPA        KKZIPA
     C*
     C     RCKY1         SETGT     PRC01
     C     RCKY2         READPE    PRC01                                  99
     C*
     C     *IN99         IFEQ      *OFF
     C                   MOVEL(P)  RCDSC1        DUDESC
     C                   MOVEL(P)  RCDSC1        DUBPRD
     C                   ENDIF
     C                   ENDIF
     C*
     C                   ENDIF
     C*
     C                   ENDSL
     C*
     C                   ENDIF                                                  DUDESC IFEQ
     C*
     C                   MOVEL     'A'           DUSTAT
     C                   MOVEL(P)  CAUSER        DUUSER
     C                   MOVEL(P)  CAPGM         DUPGM
     C                   Z-ADD     CATIME        DUTIME
     C                   Z-ADD     CADT          DUDT
     C                   MOVEL(P)  CAUSER        DUIUSR
     C                   MOVEL(P)  CAPGM         DUIPGM
     C                   Z-ADD     CAITIM        DUITIM
     C                   Z-ADD     CAIDT         DUIDT
     C*
     C                   WRITE     PDU01
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* CHKAM2 - Check amounts in upload file and convert to zero if blanks
     C*================================================================
     C     CHKAM2        BEGSR
     C*
     C     U2PAID        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2PAID
     C                   ENDIF
     C*
     C     U2CHRG        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2CHRG
     C                   ENDIF
     C*
     C     U2MRA         IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2MRA
     C                   ENDIF
     C*
     C     U2NDIS        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2NDIS
     C                   ENDIF
     C*
     C     U2DFEE        IFEQ      *BLANKS
     C                   MOVE      *ALL'0'       U2DFEE
     C                   ENDIF
     C*
     C                   IF        %trim(U2dfee) = *zeros
     C                   MOVE      *ALL'0'       U2DFEE
     C                   ENDIF
     C*
     C                   ENDSR
     C*================================================================
     C* WRTEDS - Write FDDEP record
     C*================================================================
     C     WRTEDS        BEGSR
     C*
     C                   MOVEL     ' '           XXPROC            1
     C                   Z-ADD     *HIVAL        KKSEQ#
     C     DEKY2         SETGT     PDE02
     C     DEKY1         READPE    PDE02                                  99
     C     *IN99         IFEQ      *OFF
     C     DESEQ#        ADD       1             HDSEQ#
     C                   ELSE
     C                   Z-ADD     1             HDSEQ#
     C                   ENDIF
     C*
     C* This code checks for EOB Codes/Notes in the expanded fields
     C*
     C                   SETOFF                                       72
     C*** Preparation to populate into denot1
     C                   CLEAR                   XXEDS1
     C                   CLEAR                   XXEDS2
     C                   CLEAR                   XXEDS3
     C                   CLEAR                   XXEDS4
     C*
     C                   EXSR      LODNOT
     C*
     C* Use the old fields if the new fields are not populated
     C*
     C     *IN72         IFEQ      *OFF
     C*
     C     U2EOB         IFNE      *BLANKS
     C     U2EOBD        ORNE      *BLANKS
     C                   CLEAR                   PDE01
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   ADD       1             HDSEQ#
     C*
     C     U2EOB         IFNE      *BLANKS
     C     U2EOB         CAT(P)    '-':1         DENOT1
     C                   CAT       U2EOBD:1      DENOT1
     C                   ELSE
     C                   MOVEL(P)  U2EOBD        DENOT1
     C                   ENDIF
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CADT          DEDT
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CAITIM        DEITIM
     C                   Z-ADD     CAIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C                   ENDIF
     C                   ENDIF                                                  IN72=OFF
     C*
     C     U2REAS        IFNE      *BLANKS
     C                   CLEAR                   PDE01
     C                   Z-ADD     CACO#         DECO#
     C                   Z-ADD     CASUB#        DESUB#
     C                   Z-ADD     CAFND         DEFND
     C                   Z-ADD     CAFYR         DEFYR
     C                   Z-ADD     CACASE        DECASE
     C                   MOVEL     CASS#         DESS#
     C                   Z-ADD     CAACDT        DEACDT
     C                   Z-ADD     CAEOB#        DEEOB#
     C                   Z-ADD     CADSEQ        DEDSEQ
     C                   Z-ADD     HDSEQ#        DESEQ#
     C                   MOVEL(P)  U2REAS        DENOT1
     C*
     C                   MOVEL     'A'           DESTAT
     C                   MOVEL(P)  CAUSER        DEUSER
     C                   MOVEL(P)  CAPGM         DEPGM
     C                   Z-ADD     CATIME        DETIME
     C                   Z-ADD     CADT          DEDT
     C                   MOVEL(P)  CAUSER        DEIUSR
     C                   MOVEL(P)  CAPGM         DEIPGM
     C                   Z-ADD     CAITIM        DEITIM
     C                   Z-ADD     CAIDT         DEIDT
     C                   MOVEL(P)  'U'           DEESRC
     C                   Z-ADD     EORCN         DERCN
     C                   MOVEL(P)  'Y'           DEPRNT
     C                   WRITE     PDE01
     C                   ENDIF
     C*
     C                   ENDSR
     C********************************************************************
     C     LODNOT        BEGSR
     C*
     C     XXEDS         IFNE      ' '
     C*
     C* MAKE SURE WORDS ARE NOT CUT OFF
     C*
     C                   Z-ADD     71            N1                3 0
     C     N1            DOWNE     0
     C     1             SUBST     XXEDS:N1      XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N1
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N1            SUB       1             P1                3 0
     C     P1            SUBST(P)  XXEDS:1       XXEDS1
     C     N1            ADD       70            N2                3 0
     C     N2            DOWNE     N1
     C     1             SUBST     XXEDS:N2      XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N2
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N2            SUB       N1            P2                3 0
     C     P2            SUBST(P)  XXEDS:N1      XXEDS2
     C     N2            ADD       70            N3                3 0
     C     N3            DOWNE     N2
     C     1             SUBST     XXEDS:N3      XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N3
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N3            SUB       N2            P3                3 0
     C     P3            SUBST(P)  XXEDS:N2      XXEDS3
     C     N3            ADD       70            N4                3 0
     C     N4            DOWNE     N3
     C     1             SUBST     XXEDS:N3      XXBLK             1
     C     XXBLK         IFNE      ' '
     C                   SUB       1             N4
     C                   ELSE
     C                   LEAVE
     C                   ENDIF
     C                   ENDDO
     C*
     C     N4            SUB       N3            P4                3 0
     C     P4            SUBST(P)  XXEDS:N3      XXEDS4
     C                   SETON                                        72
     C     XXPROC        IFNE      'H'
     C                   EXSR      WRTDE1
     C                   SETOFF                                       72
     C                   ENDIF
     C*
     C                   ENDIF                                                  XXEDS <>  ' '
     C*
     C                   ENDSR
     C*================================================================
     C* OVRST - OVERRIDE TO THE ADJUDICATION STATE
     C*================================================================
     C     OVRST         BEGSR
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(1)        QQCMD
     C                   MOVEL     EOAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   Z-ADD     56            LENGTH
     C                   MOVEL(P)  QCA(2)        QQCMD
     C                   MOVEL     EOAST         QQST
     C                   CALL      'QCMDEXC'
     C                   PARM                    QQCMD
     C                   PARM                    LENGTH           15 5
     C*
     C                   OPEN      FMPRL                                51
     C                   OPEN      FMSVL1                               51
     C*
     C                   ENDSR
     C*
     P*================================================================
     P* PGMSTR - Start of program processing
     P*================================================================
     PpgmStr           b
     DpgmStr           PI
      /Free
            rtvSettings(@@Cchg:@@Cifs:@@Csts:@@Cdny:@@Ccvfl:@@Cgrp:
                         @@Cemp:@@Eifs:@@Eagt:@@Eemp:@@Fifs:@@Ftyp:
                         @@Udup:@@Udrc:@@Urel:@@Upcl:@@Updn:@@Udpn:
                         @@ClDays:@@IncZro:@@Crcd:@@CrDth:@@RtCd);
                         if @@RtCd = '1';
                           isSettings = '1';
                         Else;
                           isSettings = '0';
                         Endif;
      /End-Free
     PpgmStr           e
      //===========================================================
      // SetCodeType - Set CArcFl and CAsrc based on State flag
      //===========================================================
     p SetCodeType     b
     d SetCodeType     pi
      /Free
       kkSt = EOast;
       Chain kkSt pst01;
       If %Found(ststl);
          xxMfs = STmfs;
       Else;
          xxMfs = ' ';
       Endif;
       Select;
       When EOast = 'TX' and (xxMfs = 'B' or xxMfs = 'Y');
          CArcFl = 'M';
          kkPrCd = %Subst(U2code:1:5);
          kkFdat = CAfDat;
          kkCo#  = CAco#;
          //
          // Set CAsrc based on the file where the code is found
          //
          // Ambulance
          //
          xxFound = 'N';
          SetGt (kkPrCd:kkFdat) pam01;
          Readpe kkPrcd pam01;
          Dow not %Eof(fmaml2);
             If AMstat = 'I';
                Readpe kkPrcd pam01;
             Else;
                CAsrc = 'A';
                xxFound = 'Y';
                Leave;
             Endif;
          Enddo;
          If xxFound = 'N';
          //
          // Drug Fee
          //
             SetGt (kkPrCd:kkFdat) pdf01;
             Readpe kkPrcd pdf01;
             Dow not %Eof(fmdfl1);
                If DFstat = 'I';
                   Readpe kkPrcd pdf01;
                Else;
                   CAsrc = 'F';
                   xxFound = 'Y';
                   Leave;
                Endif;
             Enddo;
          Endif;
          //
          // DME
          //
          If xxFound = 'N';
             SetGt (kkCo#:kkPrCd:kkFdat) pdm01;
             Readpe (kkCo#:kkPrcd) pdm01;
             Dow not %Eof(fmdml);
                If DMstat = 'I';
                   Readpe (kkCo#:kkPrcd) pdm01;
                Else;
                   CAsrc = 'D';
                   xxFound = 'Y';
                   Leave;
                Endif;
             Enddo;
          Endif;
          //
          // Medicaid DME
          //
          If xxFound = 'N';
             SetGt (kkPrCd:kkFdat) pme01;
             Readpe (kkPrcd) pme01;
             Dow not %Eof(ftmel2tx);
                If MEstat = 'I';
                   Readpe kkPrcd pme01;
                Else;
                   CAsrc = 'M';
                   xxFound = 'Y';
                   Leave;
                Endif;
             Enddo;
          Endif;
          //
          // RVU
          //
          If xxFound = 'N';
             SetGt (kkPrCd:kkFdat) prv01;
             Readpe (kkPrcd) prv01;
             Dow not %Eof(ftrvl3);
                If RVstat = 'I';
                   Readpe kkPrcd prv01;
                Else;
                   CAsrc = 'R';
                   xxFound = 'Y';
                   Leave;
                Endif;
             Enddo;
          Endif;
          //
          // Clinical Lab
          //
          If xxFound = 'N';
             SetGt (kkPrCd:kkFdat) plb01;
             Readpe (kkPrcd) plb01;
             Dow not %Eof(ftlbl2);
                If LBstat = 'I';
                   Readpe kkPrcd plb01;
                Else;
                   CAsrc = 'L';
                   xxFound = 'Y';
                   Leave;
                Endif;
             Enddo;
          Endif;
       Other;
       Endsl;
      /End-Free
     p SetCodeType     e
**  (QCMDEX)
OVRDBF FILE(FMPRL)   TOFILE(*LIBL/FMPRL  )
OVRDBF FILE(FMSVL1) TOFILE(*LIBL/FMSVL1  )
DLTOVR FILE(FMPRL)
DLTOVR FILE(FMSVL1)
