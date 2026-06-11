     Hcopyright('(c) 2013 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp(*CALLER)
     H***** Beg Add *** 06/16/15 **************************************
     H BNDDIR('STBNDDIR')
     H***** End Add *** 06/16/15 **************************************
    H*****************************************************************
    H*  Program: F0381RTX                                            *
    H*  Description: Dental Header Info update/display               *
    H*  Created: 05/25/06  C:079 Project: 25MNZ from F0381R          *
    H*****************************************************************
    H*  REVISIONS
     H*                                                               *
     H*  07/18/25  C:342 Project MC451                                *
     H*                  NCCI Med Data Call Enhancements              *
     H*                  DSPF changes                     .           *
     H*                  1. added error message id for sctnpi         *
     H*                  2. Changes sczip to be positive number       *
     H*                                                               *
     H*  10/01/15  C:079 PHW37605                                     *
     H*                  Check ICD-10 Indicator based on date and     *
     H*                  only in add mod                              *
     H*                                                               *
     H*  06/16/15  C:309 PHW34136 025                                 *
     H*                  Add check on NPI# so that it can contain only*
     H*                  numeric values                               *
     H*                                                               *
     H*  08/05/13 C:277 PROJECT: MC329 Phase II                       *
     H*                 Add search functionality and Validate ICD10   *
     H*                                                               *
     H*  05/23/13 C:277 PROJECT: MC329                                *
     H*                 Activate the ICD10 fields if ICD10 is active  *
     H*                                                               *
     H*  09/18/12 C:284 Project:TX079
     H*                 Add fields for Medicare Zip + 4
    H*                                                               *
     H*  01/10/12 C:251  Project: 25MSO                               *
     H*                  Add processing for Referring , Attending     *
     H*                  License number                               *
     H*                                                               *
     H*  07/07/11 C:251  Project: TX070                               *
     H*                  Add processing for Treating NPI# SCTNPI      *
     H*                  Add edits for License number                 *
     H*                                                               *
     H*  04/05/11 C:257  Support 71931                                *
     H*                  Assigning RRN to @@RRN after WRITEDBRCDF to  *
     H*                  resolve duplicate records in file FAEHP.     *
    H*
    H*  03/26/09 C:227  Support: 67149                               *
    H*                  Auto populate rendering license #.           *
    H*                                                               *
    H*  10/30/08 C:198  Project: TX048
    H*                  Added billing provider license # processing.
    H*                                                               *
    H*  03/04/08 C:062  Support: 62502
    H*                  open up lic# field if rcn                          datory.
    H*                                                               *
    H*  09/17/07 C:79   Project: 25MNZ
    H*                  WRITE SCRLIC TO DATABASE AND SCREEN
    H*
    H*  04/02/07 C:188  Support: 56671
    H*                  Commented code that was making diagnostics code mandatory.
    H*                                                               *
    H*  05/25/06 C:79   Project: 25MNZ
    H*                  Add logic for Rendering Physician Lic Number
    H*                                                               *
    H*  02/18/05 C:79   Project: TX029
    H*                  Add logic for Texas 837
    H*
    H*  03/12/03 C:079 SUPPORT: MC044 36319
    H*                 CHECK THE STATUS OF DIAG CODES
    H*
    H*  11/18/02 C:079 PROJECT: FSMNT
    H*                 CORRECT CURSOR POSITIONING FOR DIAG CODE FLDS
    H*
    H*  10/01/01 C:079 SUPPORT: MC114 28975
    H*                 REQUIRE SERVICE ZIP CODE IF AST = PA. DEFAULT
    H*                 TO ZEROS
    H*
    H*  07/06/01 C:079 PROJECT: FSMNT #196
    H*                 ADD EDIT TO SPECIALTY CODE
    H*
    H*  05/30/01 C:062 PROJECT: FS229
    H*                 POPULATE EOMB# SCREEN FIELD
    H*
    H*  05/25/01 C:062 PROJECT: FS228
    H*                 ADD SERVICE ZIP CODE
    H*
    H*  02/14/01 C:117 PROJECT: FS220
    H*                 ADD DIAGNOSIS CODES TO SCREEN
    H*
    H*  10/13/00 C:062 Support MC238
    H*                 ADD LOGIC TO DEAL WITH NON-SEARCHABLE FIELDS.
    H*
    H*  06/19/00 C:117 PROJECT FS211
    H*                 Additional Edits to date fields
    H*                 Need to update EDEDI flag.
    H*
    H*  02/21/00 C:117 PROJ: FS208
    H*                 ADDED LOGIC TO PROTECT SPECIALTY IF ON A
    H*                 RECON; ALSO SAVES SPECIALTY.
    H*
    H*  11/12/99 C:79  MC152
    H*                 Delete/Change efficiency isssue.
    H*                                                               *
    H*  09/23/99 C:56  Project: MC212                                *
    H*                 Add Provider Specialty search                 *
    H*                                                               *
    H*  09/10/99 C:56  Project: KY010                                *
    H*                 Write SCEDI to EOTAPE                         *
    H*                                                               *
    H*****************************************************************
    H*
     Ff0381ftx  cf   e             workstn
      /IF DEFINED (WEB)
     F                                     HANDLER('PROFOUNDUI(HANDLER)')
      /ENDIF
    F*
    F*  Physical file definition (for maintaining record)
     Ffaedp     uf a e             disk
     F                                     rename(ped01:dbrcdf)
    F*
    F*  EOMB Transaction File
     Ffaeop     uf   e             disk
    F*
    F*  WCMC Control File
     Ffmcxl     if   e           k disk
    F*
    F*  State Code Table File
     Fststl     if   e           k disk
    F*** BEG ADD *** 05/25/06 ****************************************
     Fststl1    if   e           k disk
     F                                     rename(pst01:pst02)
    F*** END ADD *** 05/25/06 ****************************************
    F*
    F*  Generic Table File
     Fftabl     if   e           k disk
    F*
    F*  Bill Entry Control File
     Ffmbel     if   e           k disk
    F*
    F*  Provider Master File
     Ffmpdl     if   e           k disk
    F*
    F*  EOMB Detail EOB/Notes Transaction File
     Ffaeel     if   e           k disk
     Ffaedl     if   e           k disk
     F                                     infds(dbfdsa)
    F*** BEG ADD *** 02/14/01 ****************************************
    F*  Diagnosis Code Physical File
     Ffmdil     if   e           k disk
    F*** END ADD *** 02/14/01 ****************************************
    F*** BEGIN ADD *** 05/25/01 **************************************
    F*  Geographic Zip Area File
     Ffmgzl     if   e           k disk
    F*  Provider Auxillary File
     Ffmp1l     if   e           k disk
    F*** END   ADD *** 05/25/01 **************************************
     F*** Beg Add *** 08/05/13 ***************************************
     Ffmicd10l1 if   e           k disk
     F*** End Add *** 08/05/13 ***************************************
    D*
    D*
     D dbfdsa          ds
     D dbrrna                397    400b 0
    D*================================================================
    D*  /COPY required constants
    D*================================================================
     D optnm           c                   const('Dental Header Info-
     D                                     rmation')
     D comapi          c                   const('FA007R')
     D*** Beg Chg *** 01/10/12 ***********************************************
     D*#ofscr****      c                   const(1)
     D #ofscr          c                   const(2)
     D*** End Chg *** 01/10/12 ***********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - TOP
    D*---------------------------------------------------------------------
     D ##ar            s              1
     D ##call          s              1
     D ##canc          s              1
     D ##co#           s              3  0
     D ##conm          s             40
     D ##edi           s              1
     D ##fnam          s             40
     D ##fnd           s              3  0
     D ##icd9          s                   like(diicd9)
     D ##kc            s              1
     D ##key           s                   like(stkey)
     D ##key1          s             10
     D ##kl            s              1
     D ##lost          s                   like(dilost)
     D ##mcif          s              1
     D ##med           s              1
     D ##mode          s              1
     D ##ntwk          s              1
     D ##rhab          s              1
     D ##rrn           s              9  0
     D ##rrn2          s              9  0
     D ##rtcd          s              1
     D ##sub#          s                   like(cxsub#)
     D ##ur            s              1
     D ##wcdk          s              1
     D ##wcif          s              1
     D ##wcin          s              1
     D #1case          s              7
     D #1co#           s              3
     D #1fnd           s              3
     D #1fyr           s              3
     D #1l1            s             78
     D #1l2            s             78
     D #1sub#          s              5
     D @@chrg          s              9
     D @@kc            s              1
     D @@kl            s              1
     D @@mode          s              1
     D @@nwcd          s              5
     D @@rrn           s              9  0
     D @@rrn2          s              9  0
     D hldia           s              5
     D hlspcd          s              3
     D kkast           s                   like(beast)
     D kkbtyp          s                   like(bebtyp)
     D kkcase          s                   like(edcase)
     D kkco#           s                   like(edco#)
     D kkeob#          s                   like(eoeob#)
     D kkfein          s                   like(pdfein)
     D kkfnd           s                   like(edfnd)
     D kkfyr           s                   like(edfyr)
     D kkicd9          s                   like(diicd9)
     D kklost          s                   like(dilost)
     D kkmod           s                   like(pdmod)
     D kkstyp          s                   like(bestyp)
     D kksub#          s                   like(edsub#)
     D kksys           s                   like(besys)
     D kkzipa          s              3
     D*** BEG ADD *** 05/25/06 *********************************************
     D xxst            s              2
     D xx2a            s              2
     D xx3a            s              3
     D y               s              2  0
     D*** END ADD *** 05/25/06 *********************************************
    D*---------------------------------------------------------------------
    D* Stand Alone Fields - BOTTOM
    D*---------------------------------------------------------------------
     D***Beg Add *** 05/23/13 ***************************************
     D SA015R          pr                  extpgm('SA015R')
     D @@rtcd                         1
     D*
     D xxicd10         s              1
     D @@rtcd          s              1
     D***End Add *** 05/23/13 ***************************************
     D*** Beg Add *** 08/05/13 ***************************************
     D RtvICD10Code    pr                  extpgm('F2554R')
     D incall                         1
     D inKC                           1
     D inKL                           1
     D outICD10                       7
     D inLOST                         1
     D*
     D incall          s              1
     D inKC            s              1
     D inKL            s              1
     D outICD10        s              7
     D inLOST          s              1
     D kkicd10         s                   like(iccode)
     D*** End Add *** 08/05/13 ***************************************
     D***** Beg Add *** 06/16/15 **************************************
     D Error           S              1
     D Mode            S              1
     D XXTNPI          S             30A   Inz(*Blanks)
     D*** Beg Add *** 07/18/25 ***************************************
     D invalidPos      s              2  0
      /copy sPrModule
     D*** End Add *** 07/18/25 ***************************************
     C/COPY SPRSTRING
     C***** End Add *** 06/16/15 **************************************
     C/COPY SCOPYMT01C
    C*================================================================
    C*  PLIST definitions
    C*================================================================
     C     *entry        plist
     C                   parm      *inkc         @@kc
     C                   parm      *inkl         @@kl
     C                   parm                    @@mode
     C                   parm                    @@rrn
     C                   parm                    @@rrn2
     C                   parm                    @@nwcd
     C                   parm                    @@chrg
    C*
    C*  Company file/fund (group) retrieve program parameter list
    C*
     C     compl         plist
     C                   parm                    ##co#
     C                   parm                    ##fnd
     C                   parm                    ##conm
     C                   parm                    ##fnam
    C*================================================================
    C*  KLIST definitions
    C*================================================================
    C*
    C*  Key List for WCMC Control File
    C*
     C     cxky          klist
     C                   kfld                    ##co#
     C                   kfld                    ##sub#
     C                   kfld                    ##fnd
    C*
    C** Key List for State Code Table File
    C*
     C     stky          klist
     C                   kfld                    ##key
    C*
    C** Key List for Specialty Code File
    C*
     C     spky          klist
     C                   kfld                    ##co#
     C                   kfld                    ##key
    C*
    C** Key List for Bill Entry Control FIle
    C*
     C     beky          klist
     C                   kfld                    kkast
     C                   kfld                    kksys
     C                   kfld                    kkbtyp
     C                   kfld                    kkstyp
    C** Key List for Provider Master File
    C*
     C     pdky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*
    C* KEY LIST FOR Header EOB/Notes File
    C*
     C     eeky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*
    C* KEY LIST FOR EOMB PHYS/HCFA HEADER FILE
    C*
     C     edky          klist
     C                   kfld                    ##co#
     C                   kfld                    kksub#
     C                   kfld                    kkfnd
     C                   kfld                    kkfyr
     C                   kfld                    kkcase
     C                   kfld                    kkeob#
    C*** BEG ADD *** 02/14/01 ****************************************
    C*
    C*  Key List for Diagnosis Code File
    C*
     C     diky          klist
     C                   kfld                    kkicd9
     C                   kfld                    kklost
    C*** END ADD *** 02/14/01 ****************************************
    C*** BEGIN ADD *** 05/25/01 **************************************
    C*
    C* Key List for Geographic Zip Area File
    C*
     C     gzky          klist
     C                   kfld                    ##co#
     C                   kfld                    kkzipa
    C*
    C*  Key List for Provider Auxillary File
    C*
     C     p1ky          klist
     C                   kfld                    kkfein
     C                   kfld                    kkmod
    C*** END   ADD *** 05/25/01 **************************************
     C*** Beg Add *** 08/05/13 ***************************************
     C*** KEY LIST FOR DIAGNOSIS CODE 10 MASTER FILE
     C     fmicd10l1key  klist
     C                   kfld                    kkicd10
     C                   kfld                    kklost
     C*** End Add *** 08/05/13 ***************************************
    C*
    C*================================================================
    C*  *LIKE DEFN statements
    C*================================================================
    C*
    C*
    C*================================================================
    C* DATSBR - Date check subrountine
    C*================================================================
     C     datsbr        begsr
     C                   endsr
    C*================================================================
    C* DLTPRE - Pre-processing before delete occurs
    C*================================================================
     C     dltpre        begsr
     C                   endsr
    C*================================================================
    C* DSPLYR - Processing after displaying information
    C*================================================================
     C     dsplyr        begsr
     C                   endsr
    C*================================================================
    C* ERRCK1 - Error checking for srceen 1 (SCR01)
    C*================================================================
     C     errck1        begsr
    C*** BEG ADD *** 07/06/01 ****************************************
    C*
    C** SPECIALTY CODE MUST BE VALID IF ENTERED AND NOT A RECON
    C*
     C                   if        scspcd <> *blanks
     C                             and *in29 = *off
    C*
     C                   movel(p)  scspcd        ##key
     C     spky          chain     psp01                              99
    C*
     C                   if        *in99 = *on
     C                   eval      *in90 = *on
     C                   eval      *in83 = *on
     C                   goto      ck1end
     C                   endif
    C*
     C                   endif
    C*** END ADD *** 07/06/01 ****************************************
    C*** BEGIN ADD *** 05/25/01 **************************************
    C*
    C** SERVICE ZIP CODE CANNOT BE ZERO
    C*
    C*** BEG ADD *** 10/01/01 ****************************************
     C                   if        eoast = 'PA'
     C                             and *in29 = *off
    C*** END ADD *** 10/01/01 ****************************************
    C*
     C                   if        scsvzp = *zeros
     C*** Beg Add *** 07/18/25 ***************************************
     C                             and *in29 = *off
     C*** End Add *** 07/18/25 ***************************************
     C                   eval      *in81 = *on
     C                   eval      *in90 = *on
     C                   goto      ck1end
     C                   endif
    C*** END   ADD *** 05/25/01 **************************************
    C*
    C*** BEG ADD *** 10/01/01 ****************************************
     C                   endif
    C*** END ADD *** 10/01/01 ****************************************
    C*
    C*** BEG ADD *** 02/18/05 ****************************************
    C*
    C* REQUIRE PRIMARY DIAG CODE FOR TEXAS
    C*
    C*** BEG DEL *** 04/02/07 ****************************************
    C******
    C****** REQUIRE PRIMARY DIAG CODE FOR TEXAS
    C******
    C******             if        eoast = 'TX'
    C******                       and scdia1 = *blanks
    C******             eval      *in90 = *on
    C******             eval      *in80 = *on
    C******             goto      ck1end
    C******             endif
    C*** END DEL *** 04/02/07 ****************************************
    C*
    C** VALIDATE 1ST DIAGNOSIS CODE
    C*
     C                   if        scdia1 <> *blanks
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           move(p)   scdia1        kkicd9
     C                   movel(p)  scdia1        kkicd9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa1;
          kkLost = 'D';
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in73 = *on;
                leavesr;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   movel     'D'           kklost
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *on
     C                             or distat = 'I'
     C                   eval      *in90 = *on
     C                   eval      *in73 = *on
     C                   goto      ck1end
     C                   endif
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   endif
    C*
    C** VALIDATE DIAGNOSIS CODE #2 IF ENTERED
    C*
     C                   if        scdia2 <> *blanks
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           move(p)   scdia2        kkicd9
     C                   movel(p)  scdia2        kkicd9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa2;
          kkLost = 'D';
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in74 = *on;
                leavesr;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   movel     'D'           kklost
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *on
     C                             or distat = 'I'
     C                   eval      *in90 = *on
     C                   eval      *in74 = *on
     C                   goto      ck1end
     C                   endif
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   endif
    C*
    C** VALIDATE DIAGNOSIS CODE #3 IF ENTERED
    C*
     C                   if        scdia3 <> *blanks
     C*** Beg Chg *** 05/23/13 ****************************************
     C********           move(p)   scdia3        kkicd9
     C                   movel(p)  scdia3        kkicd9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa3;
          kkLost = 'D';
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in75 = *on;
                leavesr;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   movel     'D'           kklost
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *on
     C                             or distat = 'I'
     C                   eval      *in90 = *on
     C                   eval      *in75 = *on
     C                   goto      ck1end
     C                   endif
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   endif
    C*
    C** VALIDATE DIAGNOSIS CODE #4 IF ENTERED
    C*
     C                   if        scdia4 <> *blanks
     c*** Beg Chg *** 05/23/13 ****************************************
     C********           move(p)   scdia4        kkicd9
     C                   movel(p)  scdia4        kkicd9
     C*** End Chg *** 05/23/13 ****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          kkICd10 = scDIa4;
          kkLost = 'D';
          Chain fmicd10l1key pic01;
             If not %found(fmicd10l1)
                or ICstat = 'I';
                *in90 = *on;
                *in76 = *on;
                leavesr;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   movel     'D'           kklost
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *on
     C                             or distat = 'I'
     C                   eval      *in90 = *on
     C                   eval      *in76 = *on
     C                   goto      ck1end
     C                   endif
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   endif
    C*** END ADD *** 02/18/05 ****************************************
    C*
    C** ELIGIBLE FOR DATA SUBMISSION FEILD IS REQUIRED IF IN30 IS ON
    C*          (DETERMINED IN PGMSTR SUBROUINTE)
    C*
     C*** BEG ADD *** 05/25/06 ****************************************
     C                   if        *in29 = *off
     C*** END ADD *** 05/25/06 ****************************************
     C   30              if        scedi = *blanks
     C                   eval      *in90 = *on
     C                   eval      *in78 = *on
     C                   goto      ck1end
     C                   endif
    C*
     C                   if        scedi <> *blanks
     C                             and scedi <> 'Y'
     C                             and scedi <> 'N'
     C                   eval      *in90 = *on
     C                   eval      *in79 = *on
     C                   goto      ck1end
     C                   endif
     C*** BEG ADD *** 05/25/06 ****************************************
     C                   endif
     C*** END ADD *** 05/25/06 ****************************************
    C*** BEG ADD *** 05/25/06 ****************************************
    C*
    C* RENDERING PROVIDER LICENSE NUMBER IS REQUIRED
    C*
    C* Only edit license# if Data Sub flag='Y'
    C*
     C                   if        scedi = 'Y'
    C*
    C*** BEG ADD *** 03/26/09 ****************************************
     C                   if        scflic = *blanks
     C                   seton                                        9091
     C                   goto      ck1end
     C                   endif
     C*
     C                   movel     scflic        xx2a
     C                   movel     scflic        xx3a
    C*
     C                   if        xx2a <> 'AC'
     C                             and xx2a <> 'AU'
     C                             and xx2a <> 'CR'
     C                             and xx2a <> 'DC'
     C                             and xx2a <> 'DP'
     C                             and xx2a <> 'DO'
     C                             and xx2a <> 'DS'
     C                             and xx2a <> 'IL'
     C                             and xx2a <> 'MT'
     C                             and xx2a <> 'MD'
     C                             and xx2a <> 'NP'
     C                             and xx2a <> 'OD'
     C                             and xx2a <> 'OT'
     C                             and xx2a <> 'PA'
     C                             and xx2a <> 'PT'
     C                             and xx2a <> 'SA'
     C                             and xx2a <> 'SP'
     C*** Beg Add *** 07/07/11 ************************************************
     C                             and xx2a <> 'AM'
     C                             and xx2a <> 'AS'
     C                             and xx2a <> 'CN'
     C                             and xx2a <> 'CP'
     C                             and xx2a <> 'CS'
     C                             and xx2a <> 'DM'
     C                             and xx2a <> 'DS'
     C                             and xx2a <> 'LP'
     C                             and xx2a <> 'MS'
     C                             and xx2a <> 'NF'
     C                             and xx2a <> 'OP'
     C                             and xx2a <> 'PM'
     C                             and xx2a <> 'PS'
     C                             and xx2a <> 'RA'
     C                             and xx2a <> 'RN'
     C*** End Add *** 07/07/11 ************************************************
     C                             and xx3a <> 'AMB'
     C                             and xx3a <> 'ASC'
     C                             and xx3a <> 'CNS'
     C                             and xx3a <> 'CSW'
     C                             and xx3a <> 'CPS'
     C                             and xx3a <> 'DME'
     C                             and xx3a <> 'LPC'
     C                             and xx3a <> 'LSA'
     C                             and xx3a <> 'NFA'
     C                             and xx3a <> 'PSY'
     C                             and xx3a <> 'RAD'
    C*
     C                   if        scedi = 'N'
     C                             and xx2a = 'OP'
     C                   else
     C                   eval      *in90 = *on
     C                   eval      *in92 = *on
     C                   goto      ck1end
     C                   endif
    C*
     C                   endif
    C*
     C     ' '           scan      scflic        y                        99
     C                   eval      y = y - 2
    C*
     C                   if        *in99 = *on
     C     2             subst     scflic:y      xxst
    C*
     C                   else
     C     2             subst     scflic:12     xxst
    C*               .
     C                   endif
    C*
     C                   movel(p)  xxst          ##key
     C     stky          chain     pst02                              99
    C*
     C                   if        *in99 = *on
     C                             or y <= 2
     C                   eval      *in90 = *on
     C                   eval      *in93 = *on
     C                   goto      ck1end
     C                   endif
     C                   setoff                                       919293
     C                   setoff                                       94
     C                   If        scrnlc = *blanks
     C                   Eval      scrnlc = scflic
     C                   EndIf
    C*** END ADD *** 03/26/09 ****************************************
     C*** BEG CHG *** 09/17/07 ****************************************
     C*****************  if        scflic = *blanks
     C*** BEG CHG *** 10/30/08 ****************************************
     C*****************  if        scrlic = *blanks
     C                   if        scrnlc = *blanks
     C*** END CHG *** 10/30/08 ****************************************
     C*** END CHG *** 09/17/07 ****************************************
     C                   seton                                        9071
     C                   goto      ck1end
     C                   endif
    C*
    C*
     C*** BEG CHG *** 09/17/07 ****************************************
     C*****************  movel     scflic        xx2a
     C*****************  movel     scflic        xx3a
     C*** BEG CHG *** 10/30/08 ****************************************
     C*****************  movel     scrlic        xx2a
     C*****************  movel     scrlic        xx3a
     C                   movel     scrnlc        xx2a
     C                   movel     scrnlc        xx3a
     C*** END CHG *** 10/30/08 ****************************************
     C*** END CHG *** 09/17/07 ****************************************
    C*
     C                   if        xx2a <> 'AC'
     C                             and xx2a <> 'AU'
     C                             and xx2a <> 'CR'
     C                             and xx2a <> 'DC'
     C                             and xx2a <> 'DP'
     C                             and xx2a <> 'DO'
     C                             and xx2a <> 'DS'
     C                             and xx2a <> 'IL'
     C                             and xx2a <> 'MT'
     C                             and xx2a <> 'MD'
     C                             and xx2a <> 'NP'
     C                             and xx2a <> 'OD'
     C                             and xx2a <> 'OT'
     C                             and xx2a <> 'PA'
     C                             and xx2a <> 'PT'
     C                             and xx2a <> 'SA'
     C                             and xx2a <> 'SP'
     C*** Beg Add *** 07/07/11 ************************************************
     C                             and xx2a <> 'AM'
     C                             and xx2a <> 'AS'
     C                             and xx2a <> 'CN'
     C                             and xx2a <> 'CP'
     C                             and xx2a <> 'CS'
     C                             and xx2a <> 'DM'
     C                             and xx2a <> 'DS'
     C                             and xx2a <> 'LP'
     C                             and xx2a <> 'MS'
     C                             and xx2a <> 'NF'
     C                             and xx2a <> 'OP'
     C                             and xx2a <> 'PM'
     C                             and xx2a <> 'PS'
     C                             and xx2a <> 'RA'
     C                             and xx2a <> 'RN'
     C*** End Add *** 07/07/11 ************************************************
     C                             and xx3a <> 'AMB'
     C                             and xx3a <> 'ASC'
     C                             and xx3a <> 'CNS'
     C                             and xx3a <> 'CSW'
     C                             and xx3a <> 'CPS'
     C                             and xx3a <> 'DME'
     C                             and xx3a <> 'LPC'
     C                             and xx3a <> 'LSA'
     C                             and xx3a <> 'NFA'
     C                             and xx3a <> 'PSY'
     C                             and xx3a <> 'RAD'
    C*
     C                   if        scedi = 'N'
     C                             and xx2a = 'OP'
     C                   else
     C                   eval      *in90 = *on
     C                   eval      *in72 = *on
     C                   goto      ck1end
     C                   endif
    C*
     C                   endif
    C*
     C*** BEG CHG *** 09/17/07 ****************************************
     C**** ' '           scan      scflic        y                        99
     C*** BEG CHG *** 10/30/08 ****************************************
     C*****' '           scan      scrlic        y                        99
     C     ' '           scan      scrnlc        y                        99
     C*** END CHG *** 10/30/08 ****************************************
     C*** END CHG *** 09/17/07 ****************************************
     C                   eval      y = y - 2
    C*
     C                   if        *in99 = *on
     C*** BEG CHG *** 09/17/07 ****************************************
     C**** 2             subst     scflic:y      xxst
     C*** BEG CHG *** 10/30/08 ****************************************
     C*****2             subst     scrlic:y      xxst
     C     2             subst     scrnlc:y      xxst
     C*** END CHG *** 10/30/08 ****************************************
     C*** END CHG *** 09/17/07 ****************************************
    C*
     C                   else
     C*** BEG CHG *** 09/17/07 ****************************************
     C**** 2             subst     scflic:12     xxst
     C*** BEG CHG *** 10/30/08 ****************************************
     C*****2             subst     scrlic:12     xxst
     C     2             subst     scrnlc:12     xxst
     C*** END CHG *** 10/30/08 ****************************************
     C*** END CHG *** 09/17/07 ****************************************
    C*               .
     C                   endif
    C*
     C                   movel(p)  xxst          ##key
     C     stky          chain     pst02                              99
    C*
     C                   if        *in99 = *on
     C                             or y <= 2
     C                   eval      *in90 = *on
     C                   eval      *in77 = *on
     C                   goto      ck1end
     C                   endif
    C*

    C*** END ADD *** 09/29/04 ****************************************
    C*
     C***** Beg Add *** 06/16/15 **************************************
      /Free
        // Validate Treating/Rendering NPI#
        ErTNPI=*Off;
        Mode    = 'N';
        Error   = ' ';
        XXTNPI  = %Trim(SCTNPI);
        ErTNPI = validchar(XXTNPI:Mode:Error);
        If ErTNPI = *On;
          *IN90 = *On;
          //*** Beg add ***** 07/18/25 *************************************
          mstnpi = 'MC50007';
          //*** End add ***** 07/18/25 *************************************
        EndIf;

        //*** Beg add ***** 07/18/25 *************************************
        If isModEnabled('NCCI Medical Data Call');
           // %check return non zero if non numeric value including blank
           invalidPos = %check('0123456789': sctnpi);
           If invalidPos <> 0;
              ertnpi = *on;
              mstnpi = 'MC50007';
              *in90 = *on;
              leavesr;
           Endif;
        Endif;
        //*** End add ***** 07/18/25 ************************************
      /End-Free
     C***** End Add *** 06/16/15 **************************************
     C*** BEG ADD *** 10/30/08 ****************************************
     C*** BEG DEL *** 03/26/09 ****************************************
     C*** Moved before check for Rendering License # ******************
     C******             setoff                                       919293
     C******             setoff                                       94
     C******             if        scflic = *blanks
     C******             seton                                        9091
     C******             goto      ck1end
     C******             endif
     C*
     C******             movel     scflic        xx2a
     C******             movel     scflic        xx3a
    C*
     C******             if        xx2a <> 'AC'
     C******                       and xx2a <> 'AU'
     C******                       and xx2a <> 'CR'
     C******                       and xx2a <> 'DC'
     C******                       and xx2a <> 'DP'
     C******                       and xx2a <> 'DO'
     C******                       and xx2a <> 'DS'
     C******                       and xx2a <> 'IL'
     C******                       and xx2a <> 'MT'
     C******                       and xx2a <> 'MD'
     C******                       and xx2a <> 'NP'
     C******                       and xx2a <> 'OD'
     C******                       and xx2a <> 'OT'
     C******                       and xx2a <> 'PA'
     C******                       and xx2a <> 'PT'
     C******                       and xx2a <> 'SA'
     C******                       and xx2a <> 'SP'
     C******                       and xx3a <> 'AMB'
     C******                       and xx3a <> 'ASC'
     C******                       and xx3a <> 'CNS'
     C******                       and xx3a <> 'CSW'
     C******                       and xx3a <> 'CPS'
     C******                       and xx3a <> 'DME'
     C******                       and xx3a <> 'LPC'
     C******                       and xx3a <> 'LSA'
     C******                       and xx3a <> 'NFA'
     C******                       and xx3a <> 'PSY'
     C******                       and xx3a <> 'RAD'
    C*
     C******             if        scedi = 'N'
     C******                       and xx2a = 'OP'
     C******             else
     C******             eval      *in90 = *on
     C******             eval      *in92 = *on
     C******             goto      ck1end
     C******             endif
    C*
     C******             endif
    C*
     C**** ' '           scan      scflic        y                        99
     C******             eval      y = y - 2
    C*
     C******             if        *in99 = *on
     C**** 2             subst     scflic:y      xxst
    C*
     C******             else
     C**** 2             subst     scflic:12     xxst
    C*               .
     C******             endif
    C*
     C******             movel(p)  xxst          ##key
     C**** stky          chain     pst02                              99
    C*
     C******             if        *in99 = *on
     C******                       or y <= 2
     C******             eval      *in90 = *on
     C******             eval      *in93 = *on
     C******             goto      ck1end
     C******             endif
    C*
     C*** END DEL *** 03/26/09 ****************************************
     C*** END ADD *** 10/30/08 ****************************************
     C                   endif
    C*** END ADD *** 05/25/06 ****************************************
    C*
     C     ck1end        endsr
    C*================================================================
    C* ERRCK2 - Error checking for srceen 2 (SCR02)
    C*================================================================
     C     errck2        begsr
     C*** Beg Add *** 01/10/12 *****************************************
     C                   if        scrflc <> *blanks
     C                             and scedi = 'Y'
     C                   movel     scrflc        xx2a
     C                   movel     scrflc        xx3a
    C*
     C                   if        xx2a <> 'AC'
     C                             and xx2a <> 'AU'
     C                             and xx2a <> 'CR'
     C                             and xx2a <> 'DC'
     C                             and xx2a <> 'DP'
     C                             and xx2a <> 'DO'
     C                             and xx2a <> 'DS'
     C                             and xx2a <> 'IL'
     C                             and xx2a <> 'MT'
     C                             and xx2a <> 'MD'
     C                             and xx2a <> 'NP'
     C                             and xx2a <> 'OD'
     C                             and xx2a <> 'OT'
     C                             and xx2a <> 'PA'
     C                             and xx2a <> 'PT'
     C                             and xx2a <> 'SA'
     C                             and xx2a <> 'SP'
     C                             and xx2a <> 'AM'
     C                             and xx2a <> 'AS'
     C                             and xx2a <> 'CN'
     C                             and xx2a <> 'CP'
     C                             and xx2a <> 'CS'
     C                             and xx2a <> 'DM'
     C                             and xx2a <> 'DS'
     C                             and xx2a <> 'LP'
     C                             and xx2a <> 'MS'
     C                             and xx2a <> 'NF'
     C                             and xx2a <> 'OP'
     C                             and xx2a <> 'PM'
     C                             and xx2a <> 'PS'
     C                             and xx2a <> 'RA'
     C                             and xx2a <> 'RN'
     C                             and xx3a <> 'AMB'
     C                             and xx3a <> 'ASC'
     C                             and xx3a <> 'CNS'
     C                             and xx3a <> 'CSW'
     C                             and xx3a <> 'CPS'
     C                             and xx3a <> 'DME'
     C                             and xx3a <> 'LPC'
     C                             and xx3a <> 'LSA'
     C                             and xx3a <> 'NFA'
     C                             and xx3a <> 'PSY'
     C                             and xx3a <> 'RAD'
    C*
     C                   if        scedi = 'N'
     C                             and xx2a = 'OP'
     C                   else
     C                   eval      *in90 = *on
     C                   eval      *in71 = *on
     C                   eval      *in72 = *off
     C                   goto      ck2end
     C                   endif
    C*
     C                   endif
    C*
     C     ' '           scan      scrflc        y                        99
     C                   eval      y = y - 2
    C*
     C                   if        *in99 = *on
     C     2             subst     scrflc:y      xxst
     C                   else
     C     2             subst     scrflc:12     xxst
    C*               .
     C                   endif
    C*
     C                   movel(p)  xxst          ##key
     C     stky          chain     pst02                              99
    C*
     C                   if        *in99 = *on
     C                             or y <= 2
     C                   eval      *in90 = *on
     C                   eval      *in72 = *on
     C                   eval      *in71 = *off
     C                   goto      ck2end
     C                   endif
     C                   endif
     C** Validate Attending provider license number
     C                   if        scatlc <> *blanks
     C                             and scedi = 'Y'
     C                   movel     scatlc        xx2a
     C                   movel     scatlc        xx3a
    C*
     C                   if        xx2a <> 'AC'
     C                             and xx2a <> 'AU'
     C                             and xx2a <> 'CR'
     C                             and xx2a <> 'DC'
     C                             and xx2a <> 'DP'
     C                             and xx2a <> 'DO'
     C                             and xx2a <> 'DS'
     C                             and xx2a <> 'IL'
     C                             and xx2a <> 'MT'
     C                             and xx2a <> 'MD'
     C                             and xx2a <> 'NP'
     C                             and xx2a <> 'OD'
     C                             and xx2a <> 'OT'
     C                             and xx2a <> 'PA'
     C                             and xx2a <> 'PT'
     C                             and xx2a <> 'SA'
     C                             and xx2a <> 'SP'
     C                             and xx2a <> 'AM'
     C                             and xx2a <> 'AS'
     C                             and xx2a <> 'CN'
     C                             and xx2a <> 'CP'
     C                             and xx2a <> 'CS'
     C                             and xx2a <> 'DM'
     C                             and xx2a <> 'DS'
     C                             and xx2a <> 'LP'
     C                             and xx2a <> 'MS'
     C                             and xx2a <> 'NF'
     C                             and xx2a <> 'OP'
     C                             and xx2a <> 'PM'
     C                             and xx2a <> 'PS'
     C                             and xx2a <> 'RA'
     C                             and xx2a <> 'RN'
     C                             and xx3a <> 'AMB'
     C                             and xx3a <> 'ASC'
     C                             and xx3a <> 'CNS'
     C                             and xx3a <> 'CSW'
     C                             and xx3a <> 'CPS'
     C                             and xx3a <> 'DME'
     C                             and xx3a <> 'LPC'
     C                             and xx3a <> 'LSA'
     C                             and xx3a <> 'NFA'
     C                             and xx3a <> 'PSY'
     C                             and xx3a <> 'RAD'
    C*
     C                   if        scedi = 'N'
     C                             and xx2a = 'OP'
     C                   else
     C                   eval      *in90 = *on
     C                   eval      *in73 = *on
     C                   eval      *in74 = *off
     C                   goto      ck2end
     C                   endif
    C*
     C                   endif
    C*
     C     ' '           scan      scatlc        y                        99
     C                   eval      y = y - 2
    C*
     C                   if        *in99 = *on
     C     2             subst     scatlc:y      xxst
     C                   else
     C     2             subst     scatlc:12     xxst
    C*               .
     C                   endif
    C*
     C                   movel(p)  xxst          ##key
     C     stky          chain     pst02                              99
    C*
     C                   if        *in99 = *on
     C                             or y <= 2
     C                   eval      *in90 = *on
     C                   eval      *in74 = *on
     C                   eval      *in73 = *off
     C                   goto      ck2end
     C                   endif
     C                   endif
     C*
     C*** End Add *** 01/10/12 *****************************************
     C     ck2end        endsr
    C*================================================================
    C* ERRCK3 - Error checking for srceen 3 (SCR03)
    C*================================================================
     C     errck3        begsr
     C                   endsr
    C*** BEG ADD *** 09/23/99 ****************************************
    C*================================================================
    C* KJDO - F10=SEARCH.
    C*================================================================
     C     kjdo          begsr
    C*** BEGIN DEL *** 10/13/00 **************************************
    C**********           SETOF                     3836
    C*** END   DEL *** 10/13/00 **************************************
    C*** BEG ADD *** 02/14/21 ****************************************
    C*** INITIALIZE POSITION CURSOR INDICATORS
    C*
     C                   eval      *in31 = *off
     C                   eval      *in32 = *off
     C                   eval      *in33 = *off
     C                   eval      *in34 = *off
     C                   eval      *in35 = *off
     C                   eval      *in36 = *off
     C                   eval      *in37 = *off
    C*
    C*** CLEAR HOLD FIELDS
    C*
     C                   clear                   hldia
    C*
    C*
    C*** BEG ADD *** 10/30/08 ****************************************
     C                   eval      *in80 = *off
     C                   eval      *in94 = *off
    C*** END ADD *** 10/30/08 ****************************************
    C*** BEG ADD *** 02/14/21 ****************************************
     C                   select
    C*** BEG ADD *** 02/14/01 ****************************************
    C*
    C** DIAGNOSIS CODE #1 SEARCH
    C*
     C                   when      hfld = 'SCDIA1'
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   scdia1        hldia
     C                   movel(p)  scdia1        hldia
     C*** End Chg *** 05/23/13 *****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa1 = HLDIA;
             *in90 = *on;
             *in34 = *on;
          Else;
             scDIa1 = outICD10;
             *in90 = *on;
             *in28 = *on;
          Endif;

          If scDia1 <> *blanks;
             kkICD10 = scDIa1;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD1DS = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   clear                   ##icd9
     C                   call      'F2010R'
     C                   parm      '2'           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##icd9
     C                   parm      'D'           ##lost
    C*
     C                   if        ##kl = '1'
     C                             or ##icd9 = *blanks
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   hldia         scdia1
     C                   movel(p)  hldia         scdia1
     C*** End Chg *** 05/23/13 *****************************************
     C                   eval      *in90 = *on
     C                   eval      *in34 = *on
     C                   else
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   ##icd9        scdia1
     C                   movel(p)  ##icd9        scdia1
     C*** End Chg *** 05/23/13 *****************************************
     C                   eval      *in90 = *on
     C                   eval      *in28 = *on
     C                   endif
    C*
     C                   if        scdia1 <> *blanks
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   scdia1        kkicd9
     C                   movel(p)  scdia1        kkicd9
     C*** End Chg *** 05/23/13 *****************************************
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *off
     C                   movel(p)  didsc1        scd1ds
     C                   endif
     C                   endif
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
    C*
    C** DIAGNOSIS CODE #2 SEARCH
    C*
     C                   when      hfld = 'SCDIA2'
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   scdia2        hldia
     C                   movel(p)  scdia2        hldia
     C*** End Chg *** 05/23/13 *****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa2 = HLDIA;
             *in90 = *on;
             *in28 = *on;
          Else;
             scDIa2 = outICD10;
             *in90 = *on;
             *in32 = *on;
          Endif;

          If scDia2 <> *blanks;
             kkICD10 = scDIa2;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD2DS = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   clear                   ##icd9
     C                   call      'F2010R'
     C                   parm      '2'           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##icd9
     C                   parm      'D'           ##lost
    C*
     C                   if        ##kl = '1'
     C                             or ##icd9 = *blanks
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   hldia         scdia2
     C                   movel(p)  hldia         scdia2
     C*** End Chg *** 05/23/13 *****************************************
     C                   eval      *in90 = *on
     C                   eval      *in28 = *on
     C                   else
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   ##icd9        scdia2
     C                   movel(p)  ##icd9        scdia2
     C*** End Chg *** 05/23/13 *****************************************
     C                   eval      *in90 = *on
     C                   eval      *in32 = *on
     C                   endif
    C*
     C                   if        scdia2 <> *blanks
     C*** Beg Chg *** 05/23/13 *****************************************
     C*********          move(p)   scdia2        kkicd9
     C                   movel(p)  scdia2        kkicd9
     C*** End Chg *** 05/23/13 *****************************************
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *off
     C                   movel(p)  didsc1        scd2ds
     C                   endif
     C                   endif
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
    C*
    C** DIAGNOSIS CODE #3 SEARCH
    C*
     C                   when      hfld = 'SCDIA3'
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   scdia3        hldia
     C                   movel(p)  scdia3        hldia
     C*** End Chg *** 05/23/13 *****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa3 = HLDIA;
             *in90 = *on;
             *in32 = *on;
          Else;
             scDIa3 = outICD10;
             *in90 = *on;
             *in33 = *on;
          Endif;

          If scDia3 <> *blanks;
             kkICD10 = scDIa3;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD3DS = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   clear                   ##icd9
     C                   call      'F2010R'
     C                   parm      '2'           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##icd9
     C                   parm      'D'           ##lost
    C*
     C                   if        ##kl = '1'
     C                             or ##icd9 = *blanks
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   hldia         scdia3
     C                   movel(p)  hldia         scdia3
     C*** End Chg *** 05/23/13 *****************************************
     C                   eval      *in90 = *on
     C                   eval      *in32 = *on
     C                   else
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   ##icd9        scdia3
     C                   movel(p)  ##icd9        scdia3
     C*** End Chg *** 05/23/13 *****************************************
     C                   eval      *in90 = *on
     C                   eval      *in33 = *on
     C                   endif
    C*
     C                   if        scdia3 <> *blanks
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   scdia3        kkicd9
     C                   movel(p)  scdia3        kkicd9
     C*** End Chg *** 05/23/13 *****************************************
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *off
     C                   movel(p)  didsc1        scd3ds
     C                   endif
     C                   endif
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
    C*
    C** DIAGNOSIS CODE #4 SEARCH
    C*
     C                   when      hfld = 'SCDIA4'
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   scdia4        hldia
     C                   movel(p)  scdia4        hldia
     C*** End Chg *** 05/23/13 *****************************************
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       If xxICD10 = '1' and scICdInd ='Y';
          incall = '2';
          inKC = *blanks;
          inKL = *blanks;
          outIcd10 = *blanks;
          inLost = 'D';

         // Search and select ICD10 Code
          RtvICD10Code(inCall:inKC:inKL:outICD10:inLost);

          If inKL = '1' or
             outICD10 = *blanks;
             scDIa4 = HLDIA;
             *in90 = *on;
             *in33 = *on;
          Else;
             scDIa4 = outICD10;
             *in90 = *on;
             *in38 = *on;
          Endif;

          If scDia4 <> *blanks;
             kkICD10 = scDIa4;
             Chain fmicd10l1key pic01;
                If %found(fmicd10l1);
                   SCD4DS = ICdsc1;
                Endif;
          Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
     C                   clear                   ##icd9
     C                   call      'F2010R'
     C                   parm      '2'           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##icd9
     C                   parm      'D'           ##lost
    C*
     C                   if        ##kl = '1'
     C                             or ##icd9 = *blanks
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   hldia         scdia4
     C                   movel(p)  hldia         scdia4
     C*** End Chg *** 05/23/13 *****************************************
     C                   eval      *in90 = *on
     C                   eval      *in33 = *on
     C                   else
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   ##icd9        scdia4
     C                   movel(p)  ##icd9        scdia4
     C*** End Chg *** 05/23/13 *****************************************
    C*** BEG CHG *** 11/18/02 ****************************************
    C******************** SETON                     9035  pos cursor
     C                   eval      *in90 = *on
     C                   eval      *in38 = *on
    C*** END CHG *** 11/18/02 ****************************************
     C                   endif
    C*
     C                   if        scdia4 <> *blanks
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   scdia4        kkicd9
     C                   movel(p)  scdia4        kkicd9
     C*** End Chg *** 05/23/13 *****************************************
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *off
     C                   movel(p)  didsc1        scd4ds
     C                   endif
     C                   endif
     C*** Beg Add *** 08/05/13 ****************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ****************************************
    C*
    C*** END ADD *** 02/14/01 ****************************************
     C                   when      hfld = 'SCSPCD'
     C                   movel     scspcd        hlspcd
     C                   clear                   ##key1
     C                   call      'F2335R'
     C                   parm      '2'           ##call
     C                   parm                    ##key1
     C                   parm      ' '           ##rtcd
    C*
     C                   if        ##rtcd <> '*'
     C                   move      hlspcd        scspcd
     C                   eval      *in90 = *on
     C                   eval      *in36 = *on
     C                   else
     C                   movel     ##key1        scspcd
     C                   movel     scspcd        ##key
     C     spky          chain     psp01                              99
     C                   if        *in99 = *off
     C                   movel     spdesc        scscds
     C                   endif
    C*** BEGIN CHG *** 05/25/01 **************************************
    C**********           SETON                     9038  pos cursor
     C                   eval      *in90 = *on
     C                   eval      *in35 = *on
    C*** END   CHG *** 05/25/01 **************************************
     C                   endif
    C*** BEGIN ADD *** 10/13/00 **************************************
     C                   other
    C*
     C                   select
     C                   when      hfld = 'SCEDI'
     C                   eval      *in31 = *on
    C*** BEGIN ADD *** 05/25/01 **************************************
     C                   when      hfld = 'SCSVZP'
     C                   eval      *in82 = *on
    C*** END   ADD *** 05/25/01 **************************************
    C*** BEG ADD *** 10/30/08 ****************************************
     C                   when      hfld = 'SCRNLC'
     C                   eval      *in80 = *on
     C                   when      hfld = 'SCFLIC'
     C                   eval      *in94 = *on
    C*** END ADD *** 10/30/08 ****************************************
     C*** Beg Add *** 01/10/12 *****************************************
     C                   when      hfld = 'SCRFLC'
     C                   eval      *in71 = *on
     C                   eval      *in72 = *on
     C                   when      hfld = 'SCATLC'
     C                   eval      *in73 = *on
     C                   eval      *in74 = *on
     C*** End Add *** 01/10/12 *****************************************
     C*** Beg Add *** 07/18/25 ***************************************
     C     HFLD          WHENEQ    'SCTNPI'
     C                   EVAL      MSTNPI = 'MCR0676'
     C                   EVAL      ERTNPI = *ON
     C*** End Add *** 07/18/25 ***************************************
     C                   endsl
    C*
     C                   eval      *in90 = *on
    C*** END   ADD *** 10/13/00 **************************************
     C                   endsl
     C                   endsr
    C*** END ADD *** 09/23/99 ****************************************
    C*================================================================
    C* KKDO - F11=EOB/Notes
    C*================================================================
     C     kkdo          begsr
     C                   eval      ##rrn = 0
     C                   eval      ##rrn2 = @@rrn2
     C                   call      'F0355R'
     C                   parm      ' '           ##call
     C                   parm      ' '           ##kc
     C                   parm      ' '           ##kl
     C                   parm                    ##rrn
     C                   parm                    ##rrn2
     C                   eval      *in90 = *on
     C                   endsr
    C*================================================================
    C* MVDBSC - Move data base fields to screen fields
    C*================================================================
     C     mvdbsc        begsr
    C*
     C                   clear                   scchrg
     C                   clear                   scmra
     C                   clear                   scntwk
     C                   clear                   scothr
     C                   clear                   scoth2
     C                   clear                   scpaid
     C                   clear                   scdia1
     C                   clear                   scdia2
     C                   clear                   scdia3
     C                   clear                   scdia4
     C                   clear                   scd1ds
     C                   clear                   scd2ds
     C                   clear                   scd3ds
     C                   clear                   scd4ds
    C*** BEG ADD *** 10/01/01 ****************************************
     C                   clear                   scsvzp
    C*** END ADD *** 10/01/01 ****************************************
     C*** BEG ADD *** 09/17/07 ****************************************
     C*** BEG CHG *** 10/30/08 ****************************************
     C*************      clear                   scrlic
     C                   clear                   scrnlc
     C                   clear                   scflic
     C*** END CHG *** 10/30/08 ****************************************
     C*** Beg Add *** 07/07/11 ****************************************
     C                   clear                   sctnpi
     C*** End Add *** 07/07/11 ****************************************
     C*** END ADD *** 09/17/07 ****************************************
    C*** Beg Add *** 01/10/12 ****************************************
     C                   clear                   scrflc
     C                   clear                   scatlc
    C*** End Add *** 01/10/12 ****************************************
    C*
    C* If in ADD mode and @@RRN is not zero, then user has used F12
    C* from the detail program.  This require a chain to the
    C* Header Info file in order to access the Header information.
    C*
     C                   if        @@mode = 'A'
     C                             and @@rrn <> *zeros
     C     @@rrn         chain(n)  dbrcdf                             99
     C                   endif
    C*
    C** GET PROVIDER SPECIALTY CODE (SERVICING PROVIDER)
    C*
     C                   move      eofein        kkfein
     C                   move      eomod         kkmod
    C*** BEG CHG *** 02/21/00 **(PUT CONDITION AROUND SCSPCD)*********
    C*  IF THERE IS A SPECIALTY CODE IN THE FAEOP FILE, USE IT;
    C*    OTHERWISE, USE THE ON IN FMPDP
     C                   if        eospcd <> *blanks
     C                   movel     eospcd        scspcd
     C                   else
     C     pdky          chain     ppd01                              99
     C                   if        *in99 = *off
     C                   movel     pdspcd        scspcd
     C                   endif
     C                   endif
    C*** END CHG *** 02/21/00 **(PUT CONDITION AROUND SCSPCD)*********
    C*** BEGIN ADD *** 05/25/01 **************************************
    C*
    C* Load service zip code
    C*
    C*** BEG ADD *** 10/01/01 ****************************************
     C                   if        eoast <> 'PA'
    C*** END ADD *** 10/01/01 ****************************************
     C                   clear                   scsvzp
    C*
     C                   if        eoflic <> *blanks
     C*** Beg Chg *** 09/18/12 *************************************************
     C*********          movel     eoflic        scsvzp
      /Free
       ##key = Eoast;
       Chain Stky Pst02;
       If %found(Ststl1) and Stzip4 = 'Y';
          Scsvzp = %dec(%subst(Eoflic:1:5):5:0);
          If (%subst(Eoflic:6:4) = *blanks);
             Sczip = *zeros;
          Else;
             Sczip = %dec(%subst(Eoflic:6:4):4:0);
          Endif;
       Else;
          Scsvzp = %dec(Eoflic:5:0);
       Endif;
      /End-Free
     C*** End Chg *** 09/18/12 *************************************************
     C                   else
     C     p1ky          chain     pp101                              99
     C                   if        *in99 = *off
     C                   movel     p1zip         scsvzp
     C                   endif
     C                   endif
    C*
    C*** BEG ADD *** 10/01/01 ****************************************
     C                   else
     C                   movel     eoflic        scsvzp
    C*
     C                   endif
    C*** END ADD *** 10/01/01 ****************************************
    C*** END   ADD *** 05/25/01 **************************************
    C*
     C                   movel     scspcd        ##key
     C     spky          chain     psp01                              99
     C                   if        *in99 = *off
     C                   movel     spdesc        scscds
     C                   endif
    C*
     C                   movel     eoast         scast
     C                   move(p)   eozipa        sczipa
    C*
    C* DETERMINE IF ANY EOB/NOTES EXISTS FOR THIS BILL
    C*
     C                   eval      kksub# = eosub#
     C                   eval      kkfnd = eofnd
     C                   eval      kkfyr = eofyr
     C                   eval      kkcase = eocase
     C                   eval      kkeob# = eoeob#
     C     eeky          chain     pee01                              99
     C                   if        *in99 = *off
     C                   eval      *in68 = *on
     C                   else
     C                   eval      *in68 = *off
     C                   endif
    C*** BEG ADD *** 02/14/01 ****************************************
     C                   movel     'D'           kklost
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   eddia1        scdia1
     C********           move(p)   scdia1        kkicd9
     C                   movel(p)  eddia1        scdia1
     C                   movel(p)  scdia1        kkicd9
     C*** End Chg *** 05/23/13 *****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EDicdind ='Y';
          KKICD10 = EDicd1001;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD1ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *off
     C                   movel     didsc1        scd1ds
     C                   endif
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
    C*
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   eddia2        scdia2
     C********           move(p)   scdia2        kkicd9
     C                   movel(p)  eddia2        scdia2
     C                   movel(p)  scdia2        kkicd9
     C*** End Chg *** 05/23/13 *****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EDicdind ='Y';
          KKICD10 = EDicd1002;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD2ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *off
     C                   movel     didsc1        scd2ds
     C                   endif
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
    C*
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   eddia3        scdia3
     C********           move(p)   scdia3        kkicd9
     C                   movel(p)  eddia3        scdia3
     C                   movel(p)  scdia3        kkicd9
     C*** End Chg *** 05/23/13 *****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EDicdind ='Y';
          KKICD10 = EDicd1003;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD3ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *off
     C                   movel     didsc1        scd3ds
     C                   endif
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
    C*
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   eddia4        scdia4
     C********           move(p)   scdia4        kkicd9
     C                   movel(p)  eddia4        scdia4
     C                   movel(p)  scdia4        kkicd9
     C*** End Chg *** 05/23/13 *****************************************
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       If xxICD10 = '1' and EDicdind ='Y';
          KKICD10 = EDicd1004;
          Chain fmicd10l1key pic01;
             If %found(fmicd10l1);
               scD4ds = ICdsc1;
             Endif;
       Else;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
     C     diky          chain     pdi01                              99
     C                   if        *in99 = *off
     C                   movel     didsc1        scd4ds
     C                   endif
     C*** Beg Add *** 08/05/13 ***************************************
      /Free
       Endif;
      /End-Free
     C*** End Add *** 08/05/13 ***************************************
    C*** END ADD *** 02/14/01 ****************************************
     C                   eval      scchrg = eochrg
     C                   eval      scmra = eomra
     C                   eval      scntwk = eondis
     C                   eval      scothr = eoothr
     C                   eval      scoth2 = eooth2
     C                   eval      scpaid = eopaid
     C*** BEG ADD *** 09/17/07 ****************************************
     C*** BEG CHG *** 10/30/08 ****************************************
     C**************     movel(p)  eorlic        scrlic
     C                   movel(p)  edrnlc        scrnlc
     C                   movel(p)  edflic        scflic
     C*** Beg Add *** 07/07/11 ****************************************
     C                   movel(p)  edtnpi        sctnpi
     C*** End Add *** 07/07/11 ****************************************
     C*** END CHG *** 10/30/08 ****************************************
     C*** END ADD *** 09/17/07 ****************************************
     C*** BEGIN ADD *** 03/04/08 *******************************************
     C                   setoff                                       19
     C*** BEG CHG *** 10/30/08 ****************************************
     c**************     if        scrlic=' '
     c                   if        scrnlc = ' '
     C*** END CHG *** 10/30/08 ****************************************
     c                   seton                                        19
     c                   endif
     C*** END   ADD *** 03/04/08 *******************************************
    C*
    C*** BEG ADD *** 06/19/00 ****************************************
     C                   if        ededi <> *blanks
     C                   movel     ededi         scedi
     C                   endif
    C*** END ADD *** 06/19/00 ****************************************
    C*** Beg Add *** 01/10/12 ****************************************
     C                   movel(p)  edrflc        scrflc
     C                   movel(p)  edatlc        scatlc
    C*** End Add *** 01/10/12 ****************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
          // Beg Chg *** 08/05/13 ****************************************
          // scICdind = 'N';
          // If xxicd10 = *on;
          If xxicd10 = *on and EDicdind ='Y';
          // End Chg *** 08/05/13 ****************************************
             scDIa1  = EDicd1001;
             scDIa2  = EDicd1002;
             scDIa3  = EDicd1003;
             scDIa4  = EDicd1004;
             scICdind= EDicdind ;
          // Beg Add *** 08/05/13 ****************************************
          Elseif EDicdind ='N';
             scICdind = EDicdind;
          // End Add *** 08/05/13 ****************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C                   endsr
    C*================================================================
    C* MVSCDB - Move screen fields to data base fields
    C*================================================================
     C     mvscdb        begsr
    C*
     C                   eval      edco# = eoco#
     C                   eval      edsub# = eosub#
     C                   eval      edfnd = eofnd
     C                   eval      edfyr = eofyr
     C                   eval      edcase = eocase
     C                   move      eoss#         edss#
     C                   eval      edacdt = eoacdt
    C*
    C*** BEG ADD *** 02/21/00 ****************************************
     C                   move      scspcd        eospcd
    C*** END ADD *** 02/21/00 ****************************************
    C*** BEG ADD *** 06/19/00 ****************************************
     C                   movel     scedi         ededi
    C*** END ADD *** 06/19/00 ****************************************
    C*** BEG ADD *** 02/14/01 ****************************************
     C*** Beg Chg *** 05/23/13 *****************************************
     C********           move(p)   scdia1        eddia1
     C********           move(p)   scdia2        eddia2
     C********           move(p)   scdia3        eddia3
     C********           move(p)   scdia4        eddia4
     C                   movel(p)  scdia1        eddia1
     C                   movel(p)  scdia2        eddia2
     C                   movel(p)  scdia3        eddia3
     C                   movel(p)  scdia4        eddia4
     C*** End Chg *** 05/23/13 *****************************************
    C*** END ADD *** 02/14/01 ****************************************
    C*** BEG ADD *** 10/01/01 ****************************************
     C                   movel     scsvzp        eoflic
    C*** END ADD *** 10/01/01 ****************************************
    C*** BEG ADD *** 10/30/08 ****************************************
     C**** update faedp
     C                   movel(p)  scrnlc        edrnlc
     C                   movel(p)  scflic        edflic
    C*** BEG ADD *** 10/30/08 ****************************************
     C*** Beg Add *** 07/07/11 ****************************************
     C                   movel(p)  sctnpi        edtnpi
     C*** End Add *** 07/07/11 ****************************************
    C*** Beg Add *** 01/10/12 ****************************************
     C                   movel(p)  scrflc        edrflc
     C                   movel(p)  scatlc        edatlc
    C*** End Add *** 01/10/12 ****************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free
       // Beg Chg *** 08/05/13 ****************************************
          EDicdind  = scICdind;
       // If xxICD10 = '1';
          If xxICD10 = '1' and scICdInd ='Y';
       // End Chg *** 08/05/13 ****************************************
             EDicd1001 = scDIa1;
             EDicd1002 = scDIa2;
             EDicd1003 = scDIa3;
             EDicd1004 = scDIa4;
             // If ICD10 is Active then no need of ICD9 fields
             EDdia1 = *Blanks;
             EDdia2 = *Blanks;
             EDdia3 = *Blanks;
             EDdia4 = *Blanks;
       // Beg Chg *** 08/05/13 ****************************************
          Else;
             EDdia1 = scDIa1;
             EDdia2 = scDIa2;
             EDdia3 = scDIa3;
             EDdia4 = scDIa4;

             // If ICD09 is Active then no need of ICD10 fields

             EDicd1001 = *Blanks;
             EDicd1002 = *Blanks;
             EDicd1003 = *Blanks;
             EDicd1004 = *Blanks;
       // End Chg *** 08/05/13 ****************************************
          Endif;
      /End-Free
     C*** End Add *** 05/23/13 ****************************************
     C                   endsr
    C*================================================================
    C* OTHERR - Processing for other modes
    C*================================================================
     C     otherr        begsr
     C                   endsr
    C*================================================================
    C* OTHFKS - Processing for other functions keys
    C*================================================================
     C     othfks        begsr
    C*** BEGIN ADD *** 10/13/00 **************************************
     C                   eval      *in38 = *off
     C                   eval      *in36 = *off
     C                   eval      *in31 = *off
    C*** END   ADD *** 10/13/00 **************************************
    C*** BEG ADD *** 10/30/08 **************************************
     C                   eval      *in80 = *off
     C                   eval      *in94 = *off
    C*** END ADD *** 10/30/08 ****************************************
       //*** Beg add ***** 07/18/25 *************************************
       ertnpi = *off;
       mstnpi = *blanks;
       //*** End add ***** 07/18/25 *************************************
    C*** BEG ADD *** 09/23/99 ****************************************
     C     *inkj         caseq     *on           kjdo
    C*** END ADD *** 09/23/99 ****************************************
     C     *inkk         caseq     *on           kkdo
     C                   endcs
     C                   endsr
    C*================================================================
    C* PGMEND - End of program processing
    C*================================================================
     C     pgmend        begsr
    C*
    C*  Get relative record number for header record and put in
    C*  parm @@RRN.  This will allow the ability to access header
    C*  data if user F12=Cancel from detail screen on an add.
    C*
     C                   if        @@mode = 'A'
     C     edky          chain     ped01                              99
     C                   if        *in99 = *off
     C                   eval      @@rrn = dbrrna
     C                   endif
     C                   endif
    C*
    C* The following call to the WRITER subroutine is necessary
    C* because if program bombs out in add mode before the user
    C* takes action to write the header record, 'change' mode
    C* will be looking to update a record that does not exist.
    C* Thus, if the @@RRN (FAEUP) is blanks in change mode, we must
    C* call the write subroutine to add the header record.
    C*       (WILL BE EXECUTED IF USER HITS ENTER)
    C*
     C                   if        @@mode = 'C'
     C                             and *inkc = *off
     C                             and @@rrn = *zeros
     C                   exsr      writer
     C                   endif
    C*
    C*
    C*   Call Cancel/Pend program if user hits F3=EXIT
    C*   If in Add mode, enable user to cancel bill (##CANC = Y)
    C*   Otherwise do not allow cancel capability (##CANC = N)
    C*
     C                   if        *inkc = *on
     C                             and @@mode <> 'D'
    C*
     C                   if        @@mode = 'A'
     C                   move      'Y'           ##canc
     C                   else
     C                   move      ' '           ##canc
     C                   endif
    C*
    C* If in add mode and F3=Exit is hit, call writer subroutine
    C* to write the header record
    C*
     C                   if        @@mode = 'A'
     C                             and *inkc = *on
     C                   exsr      writer
     C                   endif
    C*
    C* If in 'change' mode and F3=Exit is hit, call update subroutine
    C* to update the header record with the changed info.
    C* This will work on both SCR01 and SCR02.
    C*
     C                   if        @@mode = 'C'
     C                             and *inkc = *on
     C                             and @@rrn <> *zeros
     C                   exsr      updatr
     C                   endif
    C*
    C* The following call to the WRITER subroutine is necessary
    C* because if program bombs out in add mode before the user
    C* takes action to write the header record, 'change' mode
    C* will be looking to update a record that does not exist.
    C* Thus, if the @@RRN (FAEUP) is blanks in change mode, we must
    C* call the write subroutine to add the header record.
    C*       (WILL BE EXECUTED IF USER HITS F3=EXIT)
    C*
     C                   if        @@mode = 'C'
     C                             and @@rrn = *zeros
     C                   exsr      writer
     C                   endif
    C*
     C                   eval      ##rrn = @@rrn2
     C                   call      'F0392R'
     C     *inkl         parm      ' '           ##kl
     C                   parm      ' '           ##mode
     C                   parm                    ##rrn
     C                   parm                    ##canc
     C                   parm                    @@chrg
     C                   parm      ' '           ##rtcd
    C**********           MOVE '1'       @@KC
    C*
     C                   endif
    C*
    C*** BEG DEL *** 11/12/99 ****************************************
    C* CALL 'ADJUDICATION CALL' PROGRAM IF IN UNPEND MODE
    C* THIS IS DONE BECAUSE OTHER BILLS MAY HAVE BEEN PROCOSSED
    C* FOR THIS CASE SINCE THIS BILL HAS BEEN ADJUDICATED
    C**********
    C********** EOBLST    IFEQ 'U'
    C********** *INKL     ANDNE*ON
    C********** ##RTCD    ANDNE'P'
    C**********           Z-ADD@@RRN2    ##RRN
    C**********           CALL 'FA713R'
    C**********           PARM ' '       ##KC
    C**********           PARM ' '       ##KL
    C**********           PARM           ##RRN
    C**********           ENDIF                           EOBLST = 'U'
    C*** END DEL *** 11/12/99 ****************************************
    C*
     C                   eval      *inlr = *on
     C                   move      '1'           outflg
     C                   endsr
    C*================================================================
    C* PGMSTR - Start of program processing
    C*================================================================
     C     pgmstr        begsr
    C*
    C** Get standard screen headings
    C*
     C     @@rrn2        chain(n)  peo01                              99
    C*** BEGIN ADD *** 05/30/01 **************************************
     C                   eval      sceob# = eoeob#
    C*** END   ADD *** 05/30/01 **************************************
     C                   move      eoco#         #1co#
     C                   move      eosub#        #1sub#
     C                   move      eofnd         #1fnd
     C                   move      eofyr         #1fyr
     C                   move      eocase        #1case
     C                   clear                   ##rtcd
     C                   clear                   #1l1
     C                   clear                   #1l2
    C*
     C                   call      'FA702R'
     C                   parm                    ##rtcd
     C                   parm                    #1co#
     C                   parm                    #1sub#
     C                   parm                    #1fnd
     C                   parm                    #1fyr
     C                   parm                    #1case
     C                   parm                    #1l1
     C                   parm                    #1l2
    C*
     C                   movel     #1l1          schd1
     C                   movel     #1l2          schd2
    C*
     C                   clear                   ##rtcd
     C                   clear                   #1l1
     C                   clear                   #1l2
     C                   call      'FA707R'
     C                   parm                    ##rtcd
     C                   parm                    @@rrn2
     C                   parm                    #1l1
     C                   movel     #1l1          schd3
    C*
    C* CALL API FOR SYSTEM FLAGS
    C*
     C                   call      'SA001R'
     C                   parm      ' '           ##wcif
     C                   parm      ' '           ##mcif
     C                   parm      ' '           ##wcin
     C                   parm      ' '           ##ar
     C                   parm      ' '           ##ntwk
     C                   parm      ' '           ##ur
     C                   parm      ' '           ##med
     C                   parm      ' '           ##edi
     C                   parm      ' '           ##wcdk
     C                   parm      ' '           ##rhab
    C*
    C* SET INDICATOR TO DISPLAY NEWORK REDUCTIONS FIELD IF
    C* NETWORK IS INSTALLED
    C*
     C                   if        ##ntwk = 'Y'
     C                   eval      *in47 = *on
     C                   else
     C                   eval      *in47 = *off
     C                   endif
    C*
    C** Get 'Reductions' headings
    C*
      /Free
       clear scHMRA_dot;
       clear scHOT1_dot;
      /If Not Defined(WEB)
       scHMRA_dot = '. . . . . .';
       scHOT1_dot = '. . . . . .';
      /Endif
      /End-free
     C                   eval      ##sub# = eosub#
     C     cxky          chain     pcx01                              99
     C                   movel     cxhmra        schmra
     C                   movel     cxhnet        schnet
     C                   movel     cxhot1        schot1
     C                   movel     cxhot2        schot2
    C*
    C** DETERMINE IF ELIGIBLE FOR DATA SUBMISSION SHOULD BE DISPLAYED
    C** AND REQUIRED
    C*
     C                   movel     eoast         ##key
     C     stky          chain     pst01                              99
     C                   if        *in99 = *off
     C                             and st1a4 = 'Y'
     C                   eval      *in30 = *on
     C                   move      eoast         kkast
     C                   move      eosys         kksys
     C                   move      eobtyp        kkbtyp
     C                   movel     eostyp        kkstyp
    C*
     C     beky          chain     pbe01                              98
     C                   if        *in98 = *off
     C                   move      beedi         scedi
     C                   endif
    C*
     C                   else
    C*
     C                   eval      *in30 = *off
     C                   endif
    C*
    C** SET VARIOUS FIELDS
    C*
     C                   eval      kkco# = eoco#
     C                   eval      kksub# = eosub#
     C                   eval      kkfnd = eofnd
     C                   eval      kkfyr = eofyr
     C                   eval      kkcase = eocase
    C*
    C* DISPLAY PROVIDER NETWORK IF NOT BLANKS
    C*
     C                   if        eontwk <> *blanks
     C                   movel(p)  eontwk        scnwcd
     C                   eval      *in69 = *on
     C                   else
     C                   eval      *in69 = *off
     C                   endif
    C*
    C* SET INITIAL CURSOR POSITON TO DATE FIRST CONSULTED
    C*** BEG CHG *** 02/14/00 ****************************************
    C***********          SETON                     37
     C                   eval      *in34 = *on
    C*** END CHG *** 02/14/00 ****************************************
    C*
    C*** BEG ADD *** 02/21/00 ****************************************
    C*
    C* PROTECT SPECIALTY IF EORCN>0
    C*
     C                   if        eorcn > *zero
     C                   eval      *in29 = *on
     C                   else
    C*
     C                   eval      *in29 = *off
     C                   endif
    C*** END ADD *** 02/21/00 ****************************************
    C*** BEG ADD *** 10/01/01 ****************************************
    C*
    C* IF PA BILL AND NOT A RECON THEN POSITION CURSOR TO SERVICE ZIP
    C* AND DISPLAY IN HIGH INTENSITY.
    C*
     C                   if        eoast = 'PA'
     C                             and *in29 = *off
    C*
     C                   eval      *in38 = *on
     C                   eval      *in84 = *on
    C*
     C                   endif
    C*** END ADD *** 10/01/01 ****************************************
    C*** BEG ADD *** 02/18/05 ****************************************
    C*
    C* IF TX BILL AND NOT A RECON DISPLAY DIAG CODE IN HIGH INT.
    C*
     C                   if        eoast = 'TX'
     C                             and *in29 = *off
    C*
     C                   eval      *in39 = *on
    C*
     C                   else
     C                   eval      *in39 = *off
    C*
     C                   endif
    C*** END ADD *** 10/01/01 ****************************************
     C*** Beg Add *** 09/18/12 ************************************************
      /Free
       ##Key = Eoast;
       Chain Stky Pst02;
       If %Found(Ststl1) and Stzip4 = 'Y';
         *In42 = *On;
       Endif;
      /End-free
     C*** End Add *** 09/18/12 ************************************************
     C*** Beg Add *** 05/23/13 ****************************************
      /Free

         // Call System API to check wheather the ICD10 is active or not

         @@rtcd = *blanks;
         callp SA015R(@@rtcd);

         Select;
         // ICD10 is inactive
         When @@rtcd = '1';
         xxicd10 = '0';
         dspICdind = '1';

         // ICD10 is active
         When @@rtcd = '2';
         xxicd10 = '1';
         dspICdind = '0';
         // Beg Add *** 08/05/13 ****************************************
         // Beg Add *** 10/01/15 ****************************************
         If @@mode = 'A';
         // End Add *** 10/01/15 ****************************************
           scICdInd ='Y';
         // Beg Add *** 10/01/15 ****************************************
         Endif;
         // End Add *** 10/01/15 ****************************************
         // End Add *** 08/05/13 ****************************************
         Endsl;

         If @@MODE = 'D';
            DspicdInd = '1';
         Endif;

      /End-Free
     C*** End Add *** 05/23/13 ****************************************
    C*
     C                   endsr
    C*================================================================
    C* PREOTH - Pre-processing for other modes
    C*================================================================
     C     preoth        begsr
     C                   endsr
    C*================================================================
    C* UPDATR - Update data base record and after processing
    C*================================================================
     C     updatr        begsr
     C     @@rrn         chain     dbrcdf                             99
     C                   if        *in99 = *off
     C                   exsr      mvscdb
     C                   movel     q1user        eduser
     C                   movel     q1pgm         edpgm
     C                   time                    dsdttm
     C                   eval      edtime = dstime
    C*
     C                   eval      srdti = dsdate
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eddt
    C*
     C                   update    dbrcdf
     C                   endif
    C*
    C** UPDATE EOMB TRANSACTION FILE
    C*
     C     @@rrn2        chain     peo01                              99
     C                   if        *in99 = *off
     C                   movel(p)  scspcd        eospcd
    C*** BEG ADD *** 09/10/99 ****************************************
     C                   move      scedi         eotape
    C*** END ADD *** 09/10/99 ****************************************
     C*** BEG ADD *** 09/17/07 ****************************************
     C*** BEG DEL *** 10/30/08 ****************************************
     C*****************  movel(p)  scrlic        eorlic
     C*** END DEL *** 10/30/08 ****************************************
     C*** END ADD *** 09/17/07 ****************************************
     C*** Beg Add *** 09/18/12 *************************************************
      /Free
       Clear Eoflic;
      /End-free
     C*** End Add *** 09/18/12 *************************************************
    C*** BEGIN ADD *** 05/25/01 **************************************
    C*
     C                   if        scsvzp = *zeros
     C                   movel     '     '       eoflic
    C*** BEG ADD *** 10/01/01 ****************************************
     C     p1ky          chain     pp101                              99
     C                   if        *in99 = *off
     C                   movel     p1zip         scsvzp
     C                   endif
    C*** BEG CHG *** 10/01/01 ****************************************
    C******************** ELSE
     C                   endif
    C*** END CHG *** 10/01/01 ****************************************
     C                   movel     scsvzp        eoflic
     C                   movel(p)  eoflic        kkzipa
     C     gzky          setgt     pgz01
     C                   readp     pgz01                                  99
     C                   if        *in99 = *off
     C                             and kkzipa <= gztozp
     C                   move      gzzipa        eozipa
     C                   else
     C                   clear                   eozipa
     C                   endif
     C*** Beg Add *** 09/18/12 *************************************************
      /Free
       ##key = Eoast;
       Chain Stky Pst02;
       If %found(Ststl1) and Stzip4 = 'Y';
          Eoflic = %trim(Eoflic) + %char(Sczip);
       Endif;
      /End-Free
     C*** End Add *** 09/18/12 *************************************************
    C*** BEG DEL *** 10/01/01 ****************************************
    C******************** ENDIF
    C*** END DEL *** 10/01/01 ****************************************
    C*
    C*** END   ADD *** 05/25/01 **************************************
     C                   movel     q1user        eouser
     C                   movel     q1pgm         eopgm
     C                   eval      eotime = dstime
     C                   movel     srdto         eodt
     C                   update    peo01
     C                   endif
    C*
     C                   endsr
    C*================================================================
    C* WRITER - Write data base record and after processing
    C*================================================================
     C     writer        begsr
    C*
    C* If in ADD mode and @@RRN (FAEDP) is not zeros, then the user
    C* has used F12 from detail screen.  Must delete previous written
    C* Header record and write the new one.  This prevents having two
    C* Header records for the bill.
    C*
     C                   if        @@rrn <> *zeros
     C     @@rrn         chain     dbrcdf                             99
     C                   if        *in99 = *off
     C                   delete    dbrcdf
     C                   endif
     C                   endif
    C*
     C                   clear                   dbrcdf
     C                   exsr      mvscdb
     C                   eval      edeob# = eoeob#
     C                   move      'A'           edstat
     C                   movel     q1user        eduser
     C                   movel     q1user        ediusr
     C                   movel     q1pgm         edpgm
     C                   movel     q1pgm         edipgm
     C                   time                    dsdttm
     C                   eval      edtime = dstime
     C                   eval      editim = dstime
    C*
     C                   eval      srdti = dsdate
     C                   clear                   srdto
     C                   clear                   srerr
     C                   call      'SRCVTDAT'    dtcymd
     C                   movel     srdto         eddt
     C                   movel     srdto         edidt
    C*
     C                   write     dbrcdf
    C*** BEG ADD *** 04/05/11 ****************************************
     C     edky          chain     ped01                              99
     C                   if        *in99 = *off
     C                   eval      @@rrn = dbrrna
     C                   endif
    C*** END ADD *** 04/05/11 ****************************************
     C     @@rrn2        chain     peo01                              99
     C                   if        *in99 = *off
     C                   movel(p)  scspcd        eospcd
    C*** BEG ADD *** 09/10/99 ****************************************
     C                   move      scedi         eotape
    C*** END ADD *** 09/10/99 ****************************************
     C*** BEG ADD *** 09/17/07 ****************************************
     C*** BEG CHG *** 10/30/08 ****************************************
     C*********          movel(p)  scrlic        eorlic
     C*** update faedp
     C                   movel(p)  scrnlc        edrnlc
     C*** END CHG *** 10/30/08 ****************************************
     C*** END ADD *** 09/17/07 ****************************************
    C*** BEG ADD *** 10/01/01 ****************************************
     C                   if        scsvzp = *zeros
     C     p1ky          chain     pp101                              99
     C                   if        *in99 = *off
     C                   movel     p1zip         scsvzp
     C                   endif
     C                   endif
    C*** END ADD *** 10/01/01 ****************************************
    C*** BEGIN ADD *** 05/25/01 **************************************
      /Free
       Clear Eoflic;
      /End-Free
     C                   movel     scsvzp        eoflic
     C                   movel(p)  scsvzp        kkzipa
     C     gzky          setgt     pgz01
     C                   readp     pgz01                                  99
     C                   if        *in99 = *off
     C                             and kkzipa <= gztozp
     C                   move      gzzipa        eozipa
     C                   else
     C                   clear                   eozipa
     C                   endif
    C*** END   ADD *** 05/25/01 **************************************
     C*** Beg Add *** 09/18/12 *************************************************
      /Free
       ##key = Eoast;
       Chain Stky Pst02;
       If %found(Ststl1) and Stzip4 = 'Y';
          Eoflic = %trim(Eoflic) + %char(Sczip);
       Endif;
      /End-free
     C*** End Add *** 09/18/12 *************************************************
     C                   movel     q1user        eouser
     C                   movel     q1pgm         eopgm
     C                   eval      eotime = dstime
     C                   movel     srdto         eodt
     C                   update    peo01
     C                   endif
    C*
     C                   feod      faedp
    C*
     C                   move      '1'           outflg
     C                   endsr
