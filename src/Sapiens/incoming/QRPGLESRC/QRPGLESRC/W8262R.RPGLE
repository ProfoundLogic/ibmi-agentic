    H* (C) Copyright Progressive Data Solutions, Inc., 2006
    H*
    H/TITLE W8262R - Medical Bill Image Extract Program
    H*****************************************************************
    H*  Program Description:                                         *
    H*     Copies the Medical Bill Images to the EXPORT folder       *
    H*     process.                                                  *
    H*                                                               *
    H*  Called from: W8262C - Medical Bill Image Extract -           *
    H*                        Override Program                       *
    H*  Calls to   : None                                            *
    H*                                                               *
    H*****************************************************************
    H*  Created on 08/08/2006 by c:79 for Project 139MCV             *
    H*****************************************************************
    H*  REVISIONS:                                                   *
    H*                                                               *
     H*  09/08/21 C:79 Support: 129/97949                             *
    H*                Add prefix and root folder to path             *
    H*                                                               *
    H*  06/21/14 C:79 Redmine: 4380                                  *
    H*                -Change user to MEDREV                         *
    H*                                                               *
    H*  08/08/06 c:79 Project 139MCV                                 *
    H*                -Change user to FIRSTHEALT                     *
    H*                                                               *
    H*  07/21/05 c:79 Project 129MJR                                 *
    H*                -Change user to MEDREV                         *
    H*                                                               *
    H*  07/20/04 c:62 Project 129MBO XXXXX                           *
    H*                -Fix locking issue.                            *
    H*                                                               *
    H*  11/19/03 c:62 Project 129MBO                                 *
    H*                -Create a backup folder for audit reasons      *
    H*                                                               *
    H*  09/15/03 c:62 Project 129MCW                                 *
    H*                -Include claim# and adjuster name in file name.*
    H*                -Call W8262C1 instead of ILMCOPYC              *
    H*                                                               *
    H*****************************************************************
     Fsdcml4    uf   e           k disk
     Fimdol1    if   e           k disk
     Fwmcml1    if   e           k disk    rename(pcm01:pcm02) prefix(c1:2)
     Fsmupl1    if   e           k disk
    D*---------------------------------------------------------------------
    D* Data Structures
    D*---------------------------------------------------------------------
     D psds          esds                  extname(szq1p)
    D*
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
     D   xxsm                         2    overlay(dsdate)
     D   xxsd                         2    overlay(dsdate:3)
     D   xxsy                         2    overlay(dsdate:7)
     D                 ds
     D xxcdat                         7  0
     D  xxcc                          1  0 overlay(xxcdat)
     D  xxcy                          2  0 overlay(xxcdat:2)
     D  xxcm                          2  0 overlay(xxcdat:4)
     D  xxcd                          2  0 overlay(xxcdat:6)
     D                 ds
     D xxidoc                        12
     D  xxprefix                      8    overlay(xxidoc)
     D  xxsuffix                      4    overlay(xxidoc:9)
     D                 ds
     D xxexidoc                      25
     D  xxexprefix                    8    overlay(xxexidoc)
     D  xxunder1                      1    overlay(xxexidoc:9) inz('_')
     D  xxgroup                       3  0 overlay(xxexidoc:10)
     D  xxunder2                      1    overlay(xxexidoc:13) inz('_')
     D  xxyear                        3  0 overlay(xxexidoc:14)
     D  xxunder3                      1    overlay(xxexidoc:17) inz('_')
     D  xxclaim                       7  0 overlay(xxexidoc:18)
     D  xxunder4                      1    overlay(xxexidoc:25) inz('_')
     D                 ds
     D xxkey                         40
     D  kkco#                         3  0 overlay(xxkey)
     D  kkfnd                         3  0 overlay(xxkey:4)
     D  kkfyr                         3  0 overlay(xxkey:7)
     D  kkcase                        7  0 overlay(xxkey:10)
     D                 ds
     D @@cmt                         75
     D  @@user                       10    overlay(@@cmt:21)
     D  @@cm                          2    overlay(@@cmt:42)
     D  @@cd                          2    overlay(@@cmt:45)
     D  @@cy                          2    overlay(@@cmt:48)
     D  @@sm                          2    overlay(@@cmt:68)
     D  @@sd                          2    overlay(@@cmt:71)
     D  @@sy                          2    overlay(@@cmt:74)
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     d*** Beg Chg *** 09/08/21 *********************************************
     D*** to_path         c                   const('\EXPORT\')
     D to_path         s             80
     D to_folder       c                      const('\EXPORT\')
     d*** End Chg *** 09/08/21 *********************************************
     D to_path2        s              8a
     D datesuf         s              8a
     D ##topath2       s             90
     D cmt             s             75    dim(1) ctdata perrcd(1)
     D date_mdyy       s               d   inz datfmt(*usa)
     D kkuser          s                   like(cmuser) inz('FIRSTHEALT')
     D kkdcid          s                   like(cmdcid)
     D ##dcid          s                   like(cmdcid)
     D ##fold          s                   like(cmfold)
     D ##key           s                   like(cmkey)
     D ##fl05          s                   like(cmfl05)
     D ##topath        s             90
     D ##rtcd          s              2
     D $$asq#          s              5
     D $$cdat          s              7
     D $$cm75          s             75
     D $$code          s              2
     D $$csid          s              2
     D $$key           s             40
     D $$pgm           s             10
     D $$pnam          s             10
     D $$rmcd          s              5
     D $$secl          s              3
     D $$seq#          s              5
     D $$user          s             10
     D xxadjust        s             10
     D zzidoc          s             12    varying
     d*** Beg Add *** 09/08/21 *****************************************
     d pflr            s             80
     d rootDir         s             10
     d suffix          s              3
     d RtvSuffix       pr                  ExtPgm('SA030R')
     d                                3
     d                               80
     d RtvDocRoot      pr                  ExtPgm('RTVDOCROOT')
     d                               10
     d*** End Add *** 09/08/21 *****************************************
    C*---------------------------------------------------------------------
    C* Key Lists
    C*---------------------------------------------------------------------
    C*
     C     cmky          klist
     C                   kfld                    kkuser
    C*
     C     doky          klist
     C                   kfld                    kkdcid
    C*
     C     cmky2         klist
     C                   kfld                    kkco#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
    C*
     C     upky          klist
     C                   kfld                    c1clex
    C*---------------------------------------------------------------------
    C*  Main Processing
    C*---------------------------------------------------------------------
    C*
     C                   call      'RTVCLTID'
     C                   parm      *blanks       ##clt#            3
     C
     C                   if        ##clt# = '129'
     C                   eval      kkuser = 'MEDREV'
     C                   eval      to_path2 = '\CONDOC\'
     C                   else
     C                   eval      to_path2 = '\FIRSTH\'
     C                   endif
    C*
    C* Reading thru WMCMP
    C*
     C     cmky          chain     pcm01                              99
     C                   dow       *in99 = *off
     C                   exsr      transfer_image
     C     cmky          reade     pcm01                                  99
     C                   enddo
    C*
    C*  End of Processing
    C*
     C                   eval      *inlr = *on
    C*=====================================================================
    C* Create_path - Create the path statement for the copy operation
    C*=====================================================================
     C     Create_path   begsr
    C*
     C                   eval      xxkey = cmkey
     C                   eval      zzidoc = %trim(cmidoc)
     C                   move(p)   zzidoc        xxidoc
     C                   eval      xxexprefix = xxprefix
     C                   eval      xxgroup = kkfnd
     C                   eval      xxyear = kkfyr
     C                   eval      xxclaim = kkcase
     C                   eval      xxadjust='Unassigned'
     C     cmky2         chain     pcm02
     C                   if        %found(wmcml1)
     C     upky          chain     pup01
     C                   if        %found(smupl1)
     c                   eval      xxadjust=upusr
     C                   endif
     C                   endif
       //*** Beg Add *** 09/08/21 *****************************************
        RtvDocRoot(RootDir);
        RtvSuffix(Suffix:Pflr);
        If Pflr <> *blanks;
           Pflr = %Trim(Pflr);
        Endif;
        to_path = '/' + %Trim(Pflr) + %Trim(to_folder);
       //*** End Add *** 09/08/21 *****************************************
     C*
       //*** Beg Chg *** 09/08/21 *****************************************
     C*************      eval      ##topath = to_path + %trim(xxexidoc) +
     C*************                %trim(xxadjust) + xxsuffix
     C                   eval      ##topath = %Trim(to_path) + %trim(xxexidoc) +
     C                             %trim(xxadjust) + xxsuffix
       //*** End Chg *** 09/08/21 *****************************************
     C                   time                    dsdttm
     C                   move      dsdate        datesuf
     C                   eval      ##topath2 = to_path2 + datesuf + '_' +
     C                             xxidoc
    C*
     C                   endsr
    C*=====================================================================
    C* Transfer_Image - Copies Bill Image to EXPORT folder then updates
     C*                  diary completion date.
    C*=====================================================================
     C     Transfer_Imagebegsr
    C*
    C* Check to see if document exists
    C*
     C                   movel(p)  cmidoc        kkdcid
     C     doky          chain     pdo01                              99
     C                   if        *in99 = *off
    C*
    C* Transfer document to folder EXPORT
     C*
     C                   exsr      create_path
     C                   move      cmseq#        ##seq#
     C     doflr         cat(p)    '/':0         ##fold
     C                   cat       dosflr:0      ##fold
    C*
     C                   call      'W8262C1'
     C                   parm      cmidoc        ##dcid
     C                   parm                    ##fold
     C                   parm      cmkey         ##key
     C                   parm                    ##seq#            5
     C                   parm      cmfl05        ##fl05
     C                   parm                    ##topath
     C                   parm      *blanks       ##rtcd
     C                   parm                    ##topath2
     C*
    C* If no errors then continue else skip
     C*
     C                   if        ##rtcd = *blanks
     C*
    C* Update diary completion date
     C*
     C                   time                    dsdttm
     C                   move      dsdate        date_mdyy
     C     *cymd         move      date_mdyy     cmdcdt
     C*
     C     *cymd         move      date_mdyy     cmdt
     C                   eval      cmtime = dstime
     C                   eval      cmpgm = q1pgm
     C                   clear                   cmfl04
     C                   update    pcm01
     C*
     C*  Move values into comment string to add to additional
     C*  remarks file. (Specifying diary completion)
     C*
     C                   movel     cmt(1)        @@cmt
     C                   movel     q1user        @@user
     C                   move      cmcdat        xxcdat
     C                   movel     xxcm          @@cm
     C                   movel     xxcd          @@cd
     C                   movel     xxcy          @@cy
     C                   movel     xxsm          @@sm
     C                   movel     xxsd          @@sd
     C                   movel     xxsy          @@sy
     C*
     C* Call API to write a DARTS Additional Remark
     C*
     C                   movel(p)  cmkey         $$key
     C                   move      cmseq#        $$seq#
     C                   movel(p)  @@cmt         $$cm75
     C                   movel(p)  cmpnam        $$pnam
     C                   move      cmsecl        $$secl
     C                   movel(p)  q1user        $$user
     C                   movel(p)  q1pgm         $$pgm
     C                   movel(p)  cmrmcd        $$rmcd
     C                   move      '00000'       $$asq#
     C                   movel(p)  cmcsid        $$csid
     C                   move(p)   cmcdat        $$cdat
     C                   move(p)   cmcode        $$code
     C*
     C                   call      'DA701R'
     C                   parm                    $$key
     C                   parm                    $$seq#
     C                   parm                    $$cm75
     C                   parm                    $$pnam
     C                   parm                    $$secl
     C                   parm                    $$user
     C                   parm                    $$pgm
     C                   parm                    $$rmcd
     C                   parm                    $$asq#
     C                   parm                    $$csid
     C                   parm                    $$cdat
     C                   parm                    $$code
     C*
     C                   else
     C                   unlock    sdcml4
     C                   endif
     C                   else
     C                   unlock    sdcml4
     C                   endif
    C*
    C*
     C                   endsr
** ADDITIONAL COMMENT TABLE
DIARY COMPLETED BY:            DIARY DT:   /  /   DIARY COMPLETION:  /  /
