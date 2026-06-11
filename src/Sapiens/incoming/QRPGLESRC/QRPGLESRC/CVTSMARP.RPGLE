     H*  (C) Copyright Progressive Data Solutions, Inc., 1997
     H/TITLE CVTSMARP1 - YEAR 2000 FILE CONVERION
     H*                  HANDLE CHANGE OF WMEMP/EMEMP# & EMDIV SIZE
     H* REVISIONS:
     H*  08/19/98  C:84   PROJ:                                       *
     H*                   Change blanks in old key to zeros.          *
     F*
     F* DARTS ADDITIONAL COMMENT FILE
     FSMARP     UF   E             DISK    usropn
     FQSYSPRT   O    F   80        PRINTER OFLIND(*INOF)

      *************************************************
      ** Constants                                   **
      *************************************************
     dcnOVRDBF         c                   const('OVRDBF FILE(')                 Override command
     dcnDLTOVR         c                   const('DLTOVR FILE(')                 Delete override
     dcnCHKOBJ         c                   const('CHKOBJ OBJ(')                  Check object
     dcnTOFILE         c                   const(') TOFILE(')                    To file
     dcnOBJTYPE        c                   const(') OBJTYPE(*FILE)')             Object type

      *************************************************
      ** Work/Storage area                           **
      *************************************************
     dWorkStor         ds                  inz                                   Work/Storage
     d  xxSMAROPN                     1a   inz('0')                                SMARP open flag
     d  xxLIB                        10a                                           File library
     d  xxFILE                       10a                                           File name
     d  xxCMD                       128a                                           System command
     d  xxLENGTH                     15p 5                                         Command length
     D*
     D* DEFINE NEW DARTS KEY
     D                 DS
     D  QCKEY                  1     40
     D  QCCO#                  1      3  0
     D  QCFND                  4      6  0
     D  QCEMP#                 7     15  0
     D  QCDIV                 16     20  0
     D  QCQTE                 40     40
     D*
     D* DEFINE OLD DARTS KEY
     D                 DS
     D  OLDKEY                 1     40
     D*** BEGIN ADD *** 08/19/98 **************************************
     D  OLDZEROS               1     14
     D*** END ADD   *** 08/19/98 **************************************
     D  OLDCO#                 1      3  0
     D  OLDFND                 4      6  0
     D  OLDEMP                 7     11  0
     D  OLDDIV                12     14  0
     D  OLDSPC                15     15
     D  OLDQTE                40     40
     C*
     C     *ENTRY        PLIST
     C                   PARM                    pmBASELIB        10
     C                   PARM                    pmCLTLIB         10
     C                   PARM                    pmCOMPANY         3 0
     C* READ THE FILE
     C                   Z-ADD     0             XXCNT             7 0
     C                   READ      PAR01                                  60
     C     *IN60         DOWEQ     '0'
     C*
     C                   MOVE      *BLANKS       QCKEY
     C                   MOVE      *BLANKS       OLDKEY
     C*
     C* CHECK FOR EMPLOYER/DIV KEY
     C     ARPNAM        IFEQ      'W0050R'
     C*
     C* CHANGE THE KEY, MAKING SURE THE KEY HAS NOT ALREADY CHANGED
     C*
     C                   MOVEL     ARKEY         OLDKEY
     c*** BEGIN ADD *** 08/19/98 **************************************
     c     ' ':'0'       xlate     oldZEROS      oldZEROS
     c*** END ADD   *** 08/19/98 **************************************
     C*
     C     OLDSPC        IFEQ      ' '
     C*
     C                   Z-ADD     OLDCO#        QCCO#
     C                   Z-ADD     OLDFND        QCFND
     C                   Z-ADD     OLDEMP        QCEMP#
     C                   Z-ADD     OLDDIV        QCDIV
     C                   MOVEL     OLDQTE        QCQTE
     C*
     C                   MOVEL     QCKEY         ARKEY
     C*
     C* UPDATE THE RECORD
     C                   UPDATE    PAR01
     C                   ADD       1             XXCNT
     C*
     C                   END                                                    END OLDSPC CHK
     C*
     C                   END                                                    END PNAM CHK
     C*
     C* GET THE NEXT RECORD
     C                   READ      PAR01                                  60
     C                   END
     C*
     C                   EXCEPT    DETAIL
     C                   MOVE      '1'           *INLR

      *************************************************
      ** Program termination                         **
      *************************************************
     clr                 if        xxSMAROPN=*on                                 >-+
     clr                 eval      xxFILE='SMARP'                                  :
     clr                 exsr      $Close                                          :
     clr                 endif                                                   <-+

      *************************************************
      ** Program initialization                      **
      *************************************************
     csr   *inzsr        begsr                                                   >---+
     csr                 eval      xxSMAROPN=*off                                    :
     csr                 eval      xxLIB=pmCLTLIB                                    :
     csr                 eval      xxFILE='SMARP'                                    :
     csr                 exsr      $Check                                            :
     csr                 if        *in91=*on                                     >-+ :
     csr                 eval      xxLIB=pmBASELIB                                 : :
     csr                 exsr      $Check                                          : :
     csr                 endif                                                   <-+ :
     csr                 if        *in91=*off                                    >-+ :
     csr                 eval      xxSMAROPN=*on                                   : :
     csr                 exsr      $Open                                           : :
     csr                 endif                                                   <-+ :
     csr                 if        xxSMAROPN=*off                                >-+ :
     csr                 eval      *inLR=*on                                       : :
     csr                 endif                                                   <-+ :
     csr                 endsr                                                   <---+

      *************************************************
      ** Check for file                              **
      *************************************************
     csr   $Check        begsr                                                   >-+
     csr                 eval      xxCMD=cnCHKOBJ+%trim(xxLIB)+'/'+                :
     c                             %trim(xxFILE)+cnOBJTYPE                         :
     csr                 eval      xxLENGTH=128                                    :
     csr                 call      'QCMDEXC'                            91         :
     csr                 parm                    xxCMD                             :
     csr                 parm                    xxLENGTH                          :
     csr                 endsr                                                   <-+

      *************************************************
      ** Close a file                                **
      *************************************************
     csr   $Close        begsr                                                   >---+
     csr                 select                                                      :
     csr                 when      xxFILE='SMARP'                                >-+ :
     csr                 close     smarp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 eval      xxCMD=cnDLTOVR+%trim(xxFILE)+')'                  :
     csr                 eval      xxLENGTH=128                                      :
     csr                 call      'QCMDEXC'                            91           :
     csr                 parm                    xxCMD                               :
     csr                 parm                    xxLENGTH                            :
     csr                 endsr                                                   <---+

      *************************************************
      ** Open a file                                 **
      *************************************************
     csr   $Open         begsr                                                   >---+
     csr                 eval      xxCMD=cnOVRDBF+%trim(xxFILE)+cnTOFILE+            :
     c                             %trim(xxLIB)+'/'+%trim(xxFILE)+')'                :
     csr                 eval      xxLENGTH=128                                      :
     csr                 call      'QCMDEXC'                            91           :
     csr                 parm                    xxCMD                               :
     csr                 parm                    xxLENGTH                            :
     csr                 select                                                      :
     csr                 when      xxFILE='SMARP'                                >-+ :
     csr                 open      smarp                                91         : :
     csr                 endsl                                                   <-+ :
     csr                 endsr                                                   <---+

     OQSYSPRT   E            DETAIL         1 02
     O                                           10 'SMARP'
     O                                           30 '# OF RECS CHANGED:'
     O                       XXCNT         Z     40
