     Hcopyright('(c) 2014 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H/define CONVERT
     H*****************************************************************
     H*  DESCRIPTION:  THIS PROGRAM WILL DISPLAY A LIST OF ALL ENTRIES*
     H*                IN THE DATA AUTHORIZATION FILE FOR A PARTICULAR*
     H*                USER.                                          *
     H*                                                               *
     H*  CALLED FROM:                                                 *
     H*    W0004R02 - WCMS USER PARM MAINTENANCE                      *
     H*    F0004R02 - WCMC USER PARM MAINTENANCE                      *
     H*  CALLS TO:                                                    *
     H*    S0009R02 - DATA RESTRICION INFORMATION (OPT 2/4/5/20)      *
     H*                                                               *
     H*  CREATED: 03/05/99  PROGRAMMER: C:33  PROJECT: 48WOM/48MCB    *
     H*****************************************************************
     H*  REVISIONS:                                                   *
     H*                                                               *
     H*  02/14/14  C:279  Project: 114WKC Priss: 13                   *
     H*                   Initialize Tab name field.                  *
     H*                                                               *
     H*  01/08/14  C:279  Project: 114WKC                             *
     H*                   Add multiple groups for data restrictions.  *
     H*                                                               *
     H*  07/16/13  C:062  Support MC114 82882                        *
     H*                   Initialize fields                           *
     H*                                                               *
     H*  02/10/12  C:280  PROJECT SY291                               *
     H*                   Modify screen                               *
     H*                                                               *
     H*****************************************************************
     FS0009F    CF   E             WORKSTN
      /If Defined (WEB)
     F                                     Handler('PROFOUNDUI(HANDLER)')
      /Endif
     F                                     SFILE(SCR01S:SFRRN1)
     F                                     SFILE(SCR02S:SFRRN2)
     F                                     INFDS(FIDS01)
     F*
     F*  Physical file definition (for refreshing subfile record)
     FSMAZP     IF   E             DISK
     F                                     RENAME(PAZ01:PFRCDF)
     F*
     F*  Primary logical file definition (for displaying items)
     FSMAZL     IF   E           K DISK
     F                                     INFDS(DBFDS)
     F                                     RENAME(PAZ01:DBRCDF)
     FSMAZL1    IF   E           K DISK
     F                                     INFDS(MCFDS)
     F                                     RENAME(PAZ01:MCRCDF)
     FSMUPL     IF   E           K DISK
     FWMFNL     IF   E           K DISK    USROPN
     F                                     RENAME(PFN01:WFN01)
     FFMSML     IF   E           K DISK    USROPN
     FFMFNL     IF   E           K DISK    USROPN
     F                                     RENAME(PFN01:MFN01)
      /COPY SCOPYWW01D
     D*
     D HD              S             78    DIM(1) CTDATA PERRCD(1)              Column headings
     D HDW             S             78    DIM(1) CTDATA PERRCD(1)              WCMS Column headings
     D HDM             S             78    DIM(1) CTDATA PERRCD(1)              WCMC Column headings
      ***** beg chg ***** 01/08/14 ***************************************
     D*OF******        S              1    DIM(11) CTDATA PERRCD(1)             Options, Function
     D*OFD*****        S             35    DIM(11) ALT(OF)
     d of              s              1    dim(12) ctData perRcd(1)             Options, Function
     d ofd             s             35    dim(12) alt(of)
     d cltid           s              3
     d @@program       s             10
      ***** end chg ***** 01/08/14 ***************************************
     D*
     D***PY SCOPYWW01E
     D*
     D*
     D***PY SCOPYWW01I
     D*
     D MCFDS           DS
     D  MCRRN                397    400B 0
     D                 DS
     D  W1DATA                 1     78
     D  W1FND                  1      3
     D  W1FNAM                 5     40
     D  W1FEMP                42     50
     D  W1TEMP                52     60
     D  W1AUTH                66     66
     D  W1STAT                73     73
     D  W1NRNG                43     60
     D                 DS
     D  M1DATA                 1     78
     D  M1SUB#                 1      5
     D*
     D*** BEG CHG *** 02/10/12 ****************************************
     D*
     D*                                       7  26 M1SNAM
     D  M1SNAM                 7     21
     D*
     D*                                      28  30 M1FND
     D  M1FND                 23     25
     D*
     D*                                      32  50 M1FNAM
     D  M1FNAM                27     41
     D*
     D*                                      28  50 M1NGRP
     D  M1NGRP                23     41
     D*
     D*                                      52  60 M1FEMP
     D  M1FEMP                43     51
     D*
     D*                                      62  70 M1TEMP
     D  M1TEMP                53     61
     D*
     D  M1AUTH                66     66
     D*
     D*                                      73  73 M1STAT
     D  M1STAT                73     73
     D*
     D*                                      53  71 M1NRNG
     D  M1NRNG                43     61
     D*
     D*** END CHG *** 02/10/12 ****************************************
     D*
     D*
     D*================================================================
     D*  /COPY required constants
     D*================================================================
     D*
     D OPTNM1          C                   CONST('Data Restrictions')
     D WCMC            C                   CONST('WCMC/FSMS')
     D SLMSGI          C                   CONST('SYS0001')
     D SLMSGF          C                   CONST('SYMESG')
     D ACDPGM          C                   CONST('S0009R02')
     D COMAPI          C                   CONST('SA005R')
     D USPAPI          C                   CONST('SA005R')
     D CHGNUM          C                   CONST('SA020R')
     D NOGRP           C                   CONST('*No Group Entered*')
     D NORANG          C                   CONST('*No Range Entered*')
     D DELETD          C                   CONST('* * * Deleted')
     D INFOMX          C                   CONST(1)
     D VIEWMX          C                   CONST(1)
     D SF1PAG          C                   CONST(10)

      ***** beg add ***** 01/08/14 ***************************************
     dgetCltId         pr                  extPgm('RTVCLTID')
     d cltId                               like(cltId)

     dsetAddMult       pr                  extPgm('S0009C')
     d @@usr                               like(@@usr)
     d @@appl                              like(@@appl)
     d @@auth                              like(scAuthTyp)

     dsetGroup         pr                  extPgm('W7944C')
     d @@program                           like(@@program)
      ***** end add ***** 01/08/14 ***************************************
     I*

     C*================================================================
     C*  PLIST definitions
     C*================================================================
     C*
     C     *ENTRY        PLIST
     C                   PARM      *INKC         @@KC              1
     C                   PARM      *INKL         @@KL              1
     C                   PARM                    @@CALL            1
     C                   PARM                    @@APPL            1
     C                   PARM                    @@USR            10
     C*
     C*  Add/change/display program parameter list
     C*
     C     ACDPL         PLIST
     C     *INKC         PARM                    ##KC              1
     C     *INKL         PARM                    ##KL              1
     C                   PARM                    ##MODE            1
     C                   PARM      SCDBRR        ##RRN             9 0
     C                   PARM      @@APPL        ##APPL            1
     C                   PARM      @@USR         ##USR            10
     C*
     C*  Company file/fund (group) retrieve program parameter list
     C*
     C     COMPL         PLIST
     C                   PARM                    ##CO#             3 0
     C                   PARM                    ##FND             3 0
     C                   PARM                    ##CONM           40
     C                   PARM                    ##FNAM           40
     C*
     C     CHGPL         PLIST
     C                   PARM                    XXNUM            80
     C                   PARM                    EDTMSK           10
     C*
     C*================================================================
     C*  KLIST definitions
     C*================================================================
     C*
     C     KEY           KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@USR
     C*
     C     KEY1W         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@USR
     C                   KFLD                    KKFND
     C                   KFLD                    KKFEMP
     C*
     C     KEY1M         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@USR
     C                   KFLD                    KKSUB#
     C                   KFLD                    KKFND
     C                   KFLD                    KKFEMP
     C*
     C     AZKYW         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@USR
     C                   KFLD                    SCFND
     C*
     C     AZKYM         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    @@USR
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C     FNKYW         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCFND
     C*
     C     FNKYM         KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSUB#
     C                   KFLD                    SCFND
     C*
     C     SMKY          KLIST
     C                   KFLD                    ##CO#
     C                   KFLD                    SCSUB#
     C*
     C*================================================================
     C*  *LIKE DEFN statements
     C*================================================================
     C*
     C     *LIKE         DEFINE    AZCO#         KKCO#
     C     *LIKE         DEFINE    AZUSR         KKUSR
     C     *LIKE         DEFINE    AZSUB#        KKSUB#
     C     *LIKE         DEFINE    AZFND         KKFND
     C     *LIKE         DEFINE    AZFEMP        KKFEMP
     C     *LIKE         DEFINE    AZTEMP        XATEMP
     C     *LIKE         DEFINE    AZTEMP        XSTEMP
     C     *LIKE         DEFINE    *IN90         VLDOPT
     C*
     C*** Begin Add *** 07/16/13 *******************************************
     C                   setoff                                       07
     C                   eval      DBRRN  = *zero
     C                   eval      DBRRN2 = *zero
     C                   eval      DBRRN3 = *zero
     C                   eval      DBRRN4 = *zero
     C                   eval      DBRRN5 = *zero
     C                   eval      DBRRN6 = *zero
     C                   eval      DBRRN7 = *zero
     C                   eval      DBRRN8 = *zero
     C                   eval      DBRRN9 = *zero
     C*** End   Add *** 07/16/13 *******************************************
     C*
     C/COPY SCOPYWW01C
     C*
     C*================================================================
     C* CHKSSM - Set indicator 11 for subset message
     C*================================================================
     C*
     C     CHKSSM        BEGSR
     C                   ENDSR                                                  CHKSSM
     C*
     C*================================================================
     C* ERRCK1 - Additional errors on subfile selection
     C*================================================================
     C*
     C     ERRCK1        BEGSR
     C*
     C                   SELECT
     C*_______________________________________________________________
     C*
     C     SCSEL         WHENEQ    '20'
     C     SCSTAT        ANDEQ     'I'
     C*
     C*  EMPLOYER RANGE CAN NOT OVERLAP ANOTHER RANGE
     C*
     C  N90              SELECT
     C     @@APPL        WHENEQ    'W'
     C     AZKYW         CHAIN     DBRCDF                             60
     C     @@APPL        WHENEQ    'M'
     C     AZKYM         CHAIN     MCRCDF                             60
     C                   ENDSL
     C*
     C                   Z-ADD     SCTEMP        XSTEMP
     C*
     C     SCTEMP        IFEQ      *ZERO
     C                   MOVE      *HIVAL        XSTEMP
     C                   ENDIF                                                  SCTEMP=*ZERO
     C*
     C  N90*IN60         DOWEQ     *OFF
     C*
     C                   Z-ADD     AZTEMP        XATEMP
     C*
     C     AZTEMP        IFEQ      *ZERO
     C                   MOVE      *HIVAL        XATEMP
     C                   ENDIF                                                  AZTEMP=*ZERO
     C*
     C     XSTEMP        IFLT      AZFEMP
     C     SCFEMP        ORGT      XATEMP
     C     AZSTAT        OREQ      'I'
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C     AZKYW         READE     DBRCDF                                 60
     C     @@APPL        WHENEQ    'M'
     C     AZKYM         READE     MCRCDF                                 60
     C                   ENDSL
     C*
     C                   ELSE
     C*
     C                   SETON                                        74  90
     C                   LEAVE
     C*
     C                   ENDIF                                                  XSTEMP<AZFEMP
     C*
     C                   ENDDO                                                  *IN60=*OFF
     C*
     C     SCDBRR        CHAIN(N)  PFRCDF                             60
     C*_______________________________________________________________
     C*
     C                   ENDSL
     C*
     C                   MOVE      *ON           VLDOPT
     C*
     C                   ENDSR                                                  ERRCK1
     C*
     C*================================================================
     C* ERRCK2 - Error checking for the subset screen
     C*================================================================
     C*
     C     ERRCK2        BEGSR
     C                   ENDSR                                                  ERRCK2
     C*
     C*================================================================
     C* INZSST - Initialize the subset fields
     C*================================================================
     C*
     C     INZSST        BEGSR
     C                   ENDSR                                                  INZSST
     C*
     C*================================================================
     C* KEYINZ - Initialize key values for program start
     C*================================================================
     C*
     C     KEYINZ        BEGSR
     C*
     C                   CLEAR                   KKSUB#
     C                   CLEAR                   KKFND
     C                   CLEAR                   KKFEMP
     C*
     C                   ENDSR                                                  KEYINZ
     C*
     C*================================================================
     C* KEYSET - Set key values when doing "position to"
     C*================================================================
     C*
     C     KEYSET        BEGSR
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C                   MOVE      SCPST1        KKFND
     C     @@APPL        WHENEQ    'M'
     C                   MOVE      SCPST8        KKSUB#
     C                   MOVE      SCPST9        KKFND
     C                   ENDSL
     C*
     C                   CLEAR                   KKFEMP
     C*
     C                   ENDSR                                                  KEYSET
     C*
     C*================================================================
     C* KEYSFL - Set key values from subfile records for paging
     C*================================================================
     C*
     C     KEYSFL        BEGSR
     C*
     C                   Z-ADD     SCFND         KKFND
     C                   Z-ADD     SCSUB#        KKSUB#
     C                   Z-ADD     SCFEMP        KKFEMP
     C*
     C                   ENDSR                                                  KEYSFL
     C*
      /free
       //***** beg add ******* 01/08/14 *******************************
       //==============================================================
       // KSDO - Process F18 = Add Multiple
       //==============================================================
       Begsr ksdo;

       BtnAddCan = *off;
       BtnAddCnt = *off;
       scAuthTyp = 'A';

       Dow BtnAddCnt = *off and BtnAddCan = *off;
          ExFmt wdwAddMult;
          Select;
          When BtnAddCnt = *on;
             // setup for Add multiple data restrictions
             setAddMult(@@usr:@@appl:scAuthTyp);

             // Group selection processing
             @@program = 'S0009R';
             setGroup(@@program);

             // Refresh screen
             If *inkc = *off;
                Exsr kedo;
             Endif;
          EndSl;
       EndDo;

       Endsr;
       //***** end add ******* 01/08/14 *******************************
      /end-free
     C*================================================================
     C* MDBSC  - Move data base fields to screen fields (WCMC)
     C*================================================================
     C*
     C     MDBSC         BEGSR
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  SCSUB#        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         M1SUB#
     C*
     C                   CLEAR                   SMNAME
     C     SMKY          CHAIN     PSM01                              60
     C                   MOVEL(P)  SMNAME        M1SNAM
     C*
     C     SCFND         IFEQ      *ZERO
     C*
     C                   MOVEL(P)  NOGRP         M1NGRP
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  SCFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         M1FND
     C*
     C                   CLEAR                   FNNAME
     C     FNKYM         CHAIN     MFN01                              60
     C                   MOVEL(P)  FNNAME        M1FNAM
     C*
     C                   ENDIF                                                  SCFND=*ZERO
     C*
     C                   ENDSR                                                  MDBSC
     C*
     C*================================================================
     C* MVDBSC - Move data base fields to screen fields
     C*================================================================
     C*
     C     MVDBSC        BEGSR
     C*
     C     VLDOPT        IFEQ      *OFF
     C*
     C                   SELECT
B01  C     @@APPL        WHENEQ    'M'
     C   21              Z-ADD     MCRRN         SCDBRR
E01  C                   ENDSL
     C*
     C                   ENDIF                                                  VLDOPT=*OFF
     C*
     C                   CLEAR                   W1DATA
     C                   CLEAR                   M1DATA
     C*
     C                   Z-ADD     AZFND         SCFND
     C                   Z-ADD     AZSUB#        SCSUB#
     C                   Z-ADD     AZFEMP        SCFEMP
     C                   Z-ADD     AZTEMP        SCTEMP
     C                   MOVEL     AZSTAT        SCSTAT
     C*
     C     AZFEMP        IFEQ      *ZERO
     C     AZTEMP        ANDEQ     *ZERO
     C*
     C                   MOVEL(P)  NORANG        W1NRNG
     C                   MOVEL(P)  NORANG        M1NRNG
     C*
     C                   ELSE
     C*
     C                   MOVEL(P)  AZFEMP        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         W1FEMP
     C                   MOVEL(P)  XXNUM         M1FEMP
     C*
     C     AZFEMP        IFEQ      *ZERO
     C                   MOVE      '0'           W1FEMP
     C                   MOVE      '0'           M1FEMP
     C                   ENDIF                                                  AZFEMP=*ZERO
     C*
     C                   MOVEL(P)  AZTEMP        XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         W1TEMP
     C                   MOVEL(P)  XXNUM         M1TEMP
     C*
     C                   ENDIF                                                  AZFEMP=*ZERO=AZTEMP
     C*
     C                   MOVEL     AZAUTH        W1AUTH
     C                   MOVEL     AZSTAT        W1STAT
     C*
     C*** BEG CHG *** 02/10/12 ****************************************
     C*
     C                   MOVEL     AZAUTH        M1AUTH
     C                   MOVEL     AZSTAT        M1STAT
     C*
     C*** END CHG *** 02/10/12 ****************************************
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C                   EXSR      WDBSC
     C                   MOVEL(P)  W1DATA        SCDATA
     C     @@APPL        WHENEQ    'M'
     C                   EXSR      MDBSC
     C                   MOVEL(P)  M1DATA        SCDATA
     C                   ENDSL
     C*
     C                   ENDSR                                                  MVDBSC
     C*
     C*================================================================
     C* OPT20  - Processing for Opt 20
     C*================================================================
     C*
     C* OPTION 20=CHANGE STATUS
     C*
     C     OPT20         BEGSR
     C*
     C                   MOVE      'S'           ##MODE
     C*
     C                   CALL      ACDPGM        ACDPL                  99
     C*
     C                   ENDSR                                                  OPT20
     C*
     C*================================================================
     C* OTHFKS - Process other function keys
     C*================================================================
     C*
     C     OTHFKS        BEGSR
      /free
       //***** beg add ******* 01/08/14 *******************************
       Select;
       When *inks = *on;
          Exsr ksdo;
       Endsl;
       //***** end add ******* 01/08/14 *******************************
      /end-free
     C                   ENDSR                                                  OTHFKS
     C*
     C*================================================================
     C* OTHOPT - Process other options
     C*================================================================
     C*
     C     OTHOPT        BEGSR
     C*
     C     SCSEL         CASEQ     '20'          OPT20
     C                   ENDCS
     C*
     C                   ENDSR                                                  OTHOPT
     C*
     C*================================================================
     C* PGMEND - End of program processing
     C*================================================================
     C*
     C     PGMEND        BEGSR
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C                   CLOSE     WMFNL
     C     @@APPL        WHENEQ    'M'
     C                   CLOSE     FMSML
     C                   CLOSE     FMFNL
     C                   ENDSL
     C*
     C                   ENDSR                                                  PGMEND
     C*
     C*================================================================
     C* PGMSTR - Start of program processing
     C*================================================================
     C*
     C     PGMSTR        BEGSR
     C*
      /free
       //****** beg add ******** 02/14/14 ********************************
       clear optNm;
       //****** end add ******** 02/14/14 ********************************
      /end-free
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C                   MOVE      *OFF          *IN30
     C     'WCMS'        CAT       OPTNM1:1      OPTNM            40
     C                   MOVEA     HDW           HD
     C                   OPEN      WMFNL
     C     @@APPL        WHENEQ    'M'
     C                   MOVE      *ON           *IN30
     C     WCMC          CAT       OPTNM1:1      OPTNM
     C                   MOVEA     HDM           HD
     C                   OPEN      FMSML
     C                   OPEN      FMFNL
     C                   ENDSL
     C*
     C     @@USR         CHAIN     PUP01                              60
     C*
     C     *IN60         IFEQ      *ON
     C                   MOVEL     *BLANKS       SCUSNM
     C                   ELSE
     C     UPUSR         CAT(P)    UPUSNM:1      SCUSNM
     C                   ENDIF                                                  *IN60=*ON
     C*
      /free
       //***** beg add ***** 01/08/14 ***************************************
       getCltId(cltId);

       If cltId = '114';
          xxOpFn = '2';
          xxNmBr = '18';
          xxMis  = 'YNN';
          Exsr updofa;
       Endif;
       //***** end add ***** 01/08/14 ***************************************
      /end-free

     C                   ENDSR                                                  PGMSTR
     C*
     C*================================================================
     C* READ   - Read next record from file
     C*================================================================
     C*
     C     READ          BEGSR
     C*
     C                   MOVE      *OFF          VLDOPT
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C   21KEY           READE     DBRCDF                                 95
     C     @@APPL        WHENEQ    'M'
     C   21KEY           READE     MCRCDF                                 95
     C                   ENDSL
     C*
     C                   ENDSR                                                  READ
     C*
     C*================================================================
     C* READP  - Read previous record from file
     C*================================================================
     C*
     C     READP         BEGSR
     C*
     C                   MOVE      *OFF          VLDOPT
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C   21KEY           READPE    DBRCDF                                 95
     C     @@APPL        WHENEQ    'M'
     C   21KEY           READPE    MCRCDF                                 95
     C                   ENDSL
     C*
     C                   ENDSR                                                  READP
     C*
     C*================================================================
     C* SETGT  - Set greater than on file
     C*================================================================
     C*
     C     SETGT         BEGSR
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C   21KEY1W         SETGT     DBRCDF                             96
     C     @@APPL        WHENEQ    'M'
     C   21KEY1M         SETGT     MCRCDF                             96
     C                   ENDSL
     C*
     C                   ENDSR                                                  SETGT
     C*
     C*================================================================
     C* SETLL  - Set lower limits on file
     C*================================================================
     C*
     C     SETLL         BEGSR
     C*
     C                   SELECT
     C     @@APPL        WHENEQ    'W'
     C   21KEY1W         SETLL     DBRCDF                             96
     C     @@APPL        WHENEQ    'M'
     C   21KEY1M         SETLL     MCRCDF                             96
     C                   ENDSL
     C*
     C                   ENDSR                                                  SETLL
     C*
     C*================================================================
     C* SF1UPD - Update subfile record when item is deleted
     C*================================================================
     C*
     C     SF1UPD        BEGSR
     C*
     C                   MOVEL(P)  DELETD        SCDATA
     C*
     C                   ENDSR                                                  SF1UPD
     C*
     C*================================================================
     C* SLCTRD - Move fields for selected record to parameters
     C*================================================================
     C*
     C     SLCTRD        BEGSR
     C                   ENDSR                                                  SLCTRD
     C*
     C*================================================================
     C* SUBSET - Determine if record should be put in subfile
     C*================================================================
     C*
     C     SUBSET        BEGSR
     C                   ENDSR                                                  SUBSET
     C*
     C*================================================================
     C* WDBSC  - Move data base fields to screen fields (WCMS)
     C*================================================================
     C*
     C     WDBSC         BEGSR
     C*
     C                   MOVEL     *BLANKS       EDTMSK
     C*
     C                   MOVEL(P)  SCFND         XXNUM
     C                   CALL      CHGNUM        CHGPL
     C                   MOVEL(P)  XXNUM         W1FND
     C*
     C                   CLEAR                   FNNAME
     C     FNKYW         CHAIN     WFN01                              60
     C                   MOVEL(P)  FNNAME        W1FNAM
     C*
     C                   MOVEL(P)  W1DATA        SCDATA
     C*
     C                   ENDSR                                                  WDBSC
     C*
** Column headings
Opt
** Column headings
Opt Grp Group Name                              Employer Range    All/Inq  Sts
** Column headings
Opt  Sub# Subscriber Name Grp Group Name        Employer Range    All/Inq  Sts
** Description--------------MISN
1011=Select                 NNY
1022=Change                 YNN
1044=Delete                 YNN
1055=Display                YYY
12020=Change status         YNN
203F3=Exit                  YYN
205F5=Refresh               YYY
206F6=Add                   YNN
212F12=Cancel               YYY
217F17=Subset               NNN
218F18=Add Multiple         NNN
3
