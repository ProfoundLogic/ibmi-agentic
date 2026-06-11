     *=====================================================================
     * (c) 2011 StoneRiver, Inc. All rights reserved.
     * StoneRiver is a trademark of StoneRiver, Inc.
     *=====================================================================
    H/TITLE IS824R   UPDATE THE DOCUMENT AND FOLDER INFO FROM VALID SCAN
     *=====================================================================
     *
     * Verify the Index.
     *
     * Create the DARTS record for Index, if valid index.
     *
     * Assign to Index queue based on doc type if not already indexed.
     *
     * Write a DO record.
     *
     * Update THE Document Index record with the next available DocID index
     *  and the next Batch ID if needed.
     *
     * Update the Folder balance record with the current # of docs in
     *  subfolder and provide the creation of a new subfolder if the
     *  max has been reached.
     *
     * Created   : 07/03/00
     * Programmer: C:113
     * Project   : DR113
     *
     *=====================================================================
     *  REVISIONS:
     H*                                                               *
     H* 03/30/16 C:062 Support: 40443(114)                           *
     H*                Populate the document key list                *
     H*                                                               *
     H* 09/09/15 C:212 Project: 114MTD                               *
     H*               - Kofax OCR Bill Interface                     *
     H*
     H* 09/20/13  C:02 Optacomp/CSi Install
     **                - must write an IMIEP rcd even though it's indexed when coming from
     **                  cliams import pgms.  (ie is used by claims exit routing)
     H*                                                               *
     H* 10/17/11 C:279 Project: SY195                                *
     H*               - Add CALL to API ISA700R to remove direct     *
     H*                 access to IMDOP.                             *
     H*                                                               *
     H* 04/05/11 C:02 PROJECT 114IYP/IYT - Reapply IS724R mod         *
     H*               ADD AUTO INDEX PROCESSING                       *
     H*                                                               *
     *
     * 01/18/11 c:62 Project SEQ71656
     *               Correct Dates
     *
     * 05/30/08 c:62 Project DR257
     *               Add Rollover Processing
     *
     * 07/09/07 C:191 PROJECT: 10IJI                                *
     *               -Write to IMIEP  & IMDOP the Rcvd Date that    *
     *                has been captured in ISIM80R.                 *
     *
     *=====================================================================
      **** add 09/20/13 *****
     FSTCTL1    IF   E           K DISK
      **** End add 09/20/13 *****
    
     * Document Index Control File
     Fimdil     uf   e           k disk
    
     * Folder Naming Control File
     Fimfbl1    uf   e           k disk
    
     F***** BEG  DEL ***  10/17/11  **********************************
     * Document Transaction File
     F*****imdop     o    e             disk

     * Document Index Transaction File
     F*****imdop1    o    e             disk
     F***** END  DEL ***  10/17/11  **********************************
    
     * Index Queue File
     Fimiep     o    e             disk
    
     * Document Type File
     Fimdtl     if   e           k disk
    
     * Index Type File
     Fsmktl3    if   e           k disk
    
     * DARTS Remark File
     Fsdcml     o    e             disk
     Fsdcmli    if   e           k disk    rename(pcm01:pcm0i)

     * Document Status Table File
     Fitdsl     if   e           k disk

     * Document Notes File
     Fimdnl     if a e           k disk
     F*** BEGIN ADD *** 04/05/11 **************************************
     FIMRAP     O    E             DISK
     F*** END   ADD *** 04/05/11 **************************************
     F***** Beg  Add ***  09/09/15  **********************************
     Foptimptl1 if   e           k disk
     F***** End  Add ***  09/09/15  **********************************
    
     *=====================================================================
     * Procedure Prototypes
     *=====================================================================
      /copy IS824RINC
      /copy DA202RPR


     *=====================================================================
     * Data Structure, Table & Array Definitions
     *=====================================================================

     ** Batch note
     D fc              s             24    dim(5) ctdata perrcd(1)
    
     ** Date & Time structures
     D                 ds
     D dsdttm                        14  0
     D  dstime                        6  0 overlay(dsdttm)
     D  dsdate                        8  0 overlay(dsdttm:7)
    
     ** Program information
     D psds          esds                  extname(szq1p)
    
     *=====================================================================
     * Stand Alone Field Definitions
     *=====================================================================
     *** add 9/20/13 *************************************
     D cimpf           s              1a
     *** end add 9/20/13 *******************************
     D #NumDocs        s              5  0
     D batchSeq#       s              4  0
     D numBytes        s              9  0
     D docSeq#         s              6  0
     D docSeq2         s              6  0
     D fSecByLvl       s              1a
     D secLvl          s              3  0
     D sequence        s              5  0
     D eNumDocs        s              5a
     D eBatchId        s             12a
     D eBatchSeq       s              4a
     D batchSts        s              2a
     D eNumBytes       s              9a
     D eImgFldr        s              8a
     D eImgSfldr       s             12a
     D eDocId          s             12a
     D eDocType        s              8a
     D eDocSeq         s              6a
     D eKey            s             40a
     D eNumPages       s              4a
     D path            s             60a
     D errorCode       s              1a
     D eRtnCode        s              1a
     D eScanDate       s              8a
     *** BEG ADD ********** 07/09/07 ********************************
     D eRcvdDate       s              8a
     *** END ADD ********** 07/09/07 ********************************
     D eSequence       s              5a
     D eScanTime       s              6a
     D eWsId           s              2a
     d xDate           s               d
     d xTime           s               t
     d dateChar        s             10a
     d timeChar        s              8a
     D #ScanDate       s              8  0
     *** BEG ADD ********** 07/09/07 ********************************
     D #RcvdDate       s              8  0
     *** END ADD ********** 07/09/07 ********************************
     D noteSeq         s              5  0
     D indexPgm        s             10a
     D xPrefix         s              9a
     D xSuffix         s              3a
     D #Suffix         s              3  0

     ** DARTS API parameters
     D pkey            s             40a
     d pcomm           s            256a
     d pcdat           s              8a
     d pdcid           s             12a
     d ppnam           s             10a
     d puser           s             10a
     d psecl           s              3a
     d ppgm            s             10a
     d pfold           s             80a
     d pdifl           s              1a
     d pipgm           s             10a
     d pfl01           s              1a
     d pfl02           s              1a
     d prmcd           s              5a
     d pcom2           s             19a
     d papid           s              2a
     d pidoc           s             12a
     d pique           s             10a
     d psequence       s              5a
     d pimgf           s              1a
     d pipth           s             10a
     d ppidx           s              5a
     d pqpty           s              1a
     d pdctp           s              5a
     d pcsid           s              2a
     d pcode           s              2a

     *=====================================================================
     * I specs
     *=====================================================================
     Ipcm0i
     I              cmkey                       pikey
     I              cmseq#                      piseq#
    
     *=====================================================================
     * KList Definitions
     *=====================================================================
      **** add 09/20/13 ***************************

     C     CTKY          KLIST
     C                   KFLD                    dtrmcd
      *
      **** end add 09/20/13 ***************************
     ** Document Index Control File
     C     diky1         klist
     C                   kfld                    eWsId
    
     ** Folder Balance file
     C     fbky1         klist
     C                   kfld                    eImgFldr
    
     ** Document Type file
     C     dtky1         klist
     C                   kfld                    iedoct
    
     C     dtky2         klist
     C                   kfld                    eDocType
    
     ** Key Type file
     C     ktky1         klist
     C                   kfld                    indexPgm
    
     ** DARTS Remark File
     C     cmkya         klist
     C                   kfld                    eKey
     C                   kfld                    sequence
    
     ** Batch Status Table file
     C     dsky1         klist
     C                   kfld                    batchSts
    
     ** DOC   NOTES (HISTORY) FILE
     C     dnky1         klist
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****              kfld                    dodcid
     C                   kfld                    qqdcid
     C***** END  CHG ***  10/17/11  **********************************
     C                   kfld                    noteSeq
    
     C     dnky2         klist
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****              kfld                    dodcid
     C                   kfld                    qqdcid
     C***** END  CHG ***  10/17/11  **********************************
    
     *=====================================================================
     * PList Definitions
     *=====================================================================
     ** ENTRY PARAMETERS
     C     *entry        plist
     C                   parm                    eWsId
     C                   parm                    eDocSeq
     C                   parm                    eBatchSeq
     C                   parm                    eImgFldr
     C                   parm                    eImgSfldr
     C                   parm                    eNumDocs
     C                   parm                    eDocType
     C                   parm                    eDocId
     C                   parm                    eNumBytes
     C                   parm                    eScanDate
     C                   parm                    eScanTime
     C                   parm                    eBatchId
     C                   parm                    eNumPages
     C                   parm                    eKey
     C                   parm                    eSequence
     C                   parm                    eRtnCode
     *** BEG ADD ********** 07/09/07 ********************************
     C                   parm                    eRcvdDate
     *** END ADD ********** 07/09/07 ********************************
    
      ****  add 9/20/13 ***************************
      **  input a parm if from the claims import pgms so we can write an IMIEP rcd
      **   even for indexed items
      **
     C                   if        eRtnCode = 'X'
     C                   eval      cimpf = eRtnCode
     C                   eval      eRtnCode = *blanks
     C                   endif
      **
      **
      **** end add 9/20/13 **************************
     *=====================================================================
     * Main
     *=====================================================================
      ** Date check/conversion parameters
     C     DTCYMD        PLIST
     C                   PARM                    SRDTI             8 0
     C                   PARM      *BLANKS       SRFFMT            8
     C                   PARM      '*CYMD'       SRTFMT            8
     C                   PARM                    SRDTO            10
     C                   PARM                    SRERR             1
      *** END   ADD *** 01/04/00 ***********************************************
    
     C***** BEG  ADD ***  10/17/11  **********************************
     C     ISAKEY        PLIST
     C                   PARM                    QQDCID           12
     C                   PARM                    QQFLR             8
     C                   PARM                    QQSFLR           12
     C                   PARM                    QQOSYS            1
     C                   PARM                    QQLBN            10
     C                   PARM                    QQOFLD            8
     C                   PARM                    QQOSFL           12
     C                   PARM                    QQODCI           12
     C                   PARM                    QQARQS            4 0
     C                   PARM                    QQDTYP            8
     C                   PARM                    QQBYTD            5 1
     C                   PARM                    QQWSID            2
     C                   PARM                    QQSDT             8 0
     C                   PARM                    QQSTMD            6 0
     C                   PARM                    QQSREP            8
     C                   PARM                    QQBCHI           12
     C                   PARM                    QQPGS             4 0
     C                   PARM                    QQAIFL            1
     C                   PARM                    QQOPTF            1
     C                   PARM                    QQIDFX            1
     C                   PARM                    QQVOL1           10
     C                   PARM                    QQVOL2           10
     C                   PARM                    QQVOL3           10
     C                   PARM                    QQDKEY           40
     C                   PARM                    QQDSQ#            5 0
     C                   PARM                    QQFL01            1
     C                   PARM                    QQFL02            1
     C                   PARM                    QQFL03            1
     C                   PARM                    QQFL04            1
     C                   PARM                    QQFL05            1
     C                   PARM                    QQCHR5            5
     C                   PARM                    QQDSTS            2
     C                   PARM                    QQRCVD            8 0
     C                   PARM                    QQUNCN          255
     C                   PARM                    QQINDX            1
     C                   PARM                    QQRTRN            1
     C***** END  ADD ***  10/17/11  **********************************
     C*
     ** Set up the image path
     c                   eval      path = eImgFldr + '/' + eImgSfldr

     ** Validate the index information
     C                   exsr      srvalidx
    
     ** Write the IMDOP records
     C                   if        errorCode = *blanks
     C                   exsr      srwrtdo
     C                   endif
    
     ** Write the batch notes
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****              if        dodsts = 'CM'
     C                   if        qqdsts = 'CM'
     C***** END  CHG ***  10/17/11  **********************************
     C                             and errorCode = *blanks
     C                   exsr      bnotes
     C                   endif
    
     ** Set up for index queue if not indexed
     C                   if        eKey = *blanks
     C                             and errorCode = *blanks
     **** add 9/20/13 ***********************************
     C                             or cimpf <> *blanks
     **** add 9/20/13 ***********************************
     C                   exsr      srnotidx
     C                   endif
    
     ** Update the IMDIP records
     C                   if        errorCode = *blanks
     C                   exsr      srupddi
     C                   endif

     ** Perform folder balancing
     C                   if        errorCode = *blanks
     C                   exsr      srfldbal
     C                   endif

     C                   eval      eRtnCode = errorCode
    
     C                   move      *on           *inlr

     *=====================================================================
     * *inzsr - Initialization subroutine
     *=====================================================================
     c     *inzsr        begsr

     c                   clear                   errorCode
     c                   move      eScanDate     #ScanDate
     *** BEG ADD ********** 07/09/07 ********************************
     c                   if        q1prms>16
     c                   move      eRcvdDate     #RcvdDate
     c                   endif
     *** END ADD ********** 07/09/07 ********************************

     c     endinzsr      endsr


     C*** BEGIN ADD *** 04/05/11 **************************************
     C*================================================================
     C* BARIDX - BARCODE INDEX PROCESSING
     C*================================================================
     C     BARIDX        BEGSR
     C*
     C* WRITE IMRAP RECORD
     C*
     C                   CLEAR                   PRA01
     C*
     C                   MOVEL(P)  edocid        RADCID
     C                   MOVEL(P)  ekey          RADKEY
     C*
     C                   TIME                    DSDTTM
     C                   MOVEL(P)  Q1USER        RALUSR
     C                   MOVEL(P)  Q1PGM         RALPGM
     C                   Z-ADD     DSTIME        RALTIM
     C                   Z-ADD     DSDATE        SRDTI
     C                   CALL      'SRCVTDAT'    DTCYMD
     C                   MOVEL     SRDTO         RALDT
     C                   MOVEL(P)  RALUSR        RAIUSR
     C                   MOVEL(P)  RALPGM        RAIPGM
     C                   Z-ADD     RALTIM        RAITIM
     C                   Z-ADD     RALDT         RAIDT
     C*
     C                   WRITE     PRA01
     C*
     C                   ENDSR
     C*** END   ADD *** 04/05/11 **************************************
     *=====================================================================
     * newsfl - Subroutine to create a new subfolder
     *=====================================================================
     C     newsfl        begsr
    
     C                   move      fbcisf        #Suffix
     C                   eval      #Suffix = #Suffix + 1
     C                   movel     fbcisf        xPrefix
     C                   move      #Suffix       xSuffix
     c                   eval      fbcisf = %trim(xPrefix) + %trim(xSuffix)
    
     ** Call CL pgm to create new subfolder
     C                   call      'ISADDF'
     C                   parm                    fbcif
     C                   parm                    fbcisf
     C                   parm                    errorCode
    
     C                   eval      fbc#ds = 0
    
     C     endnewsfl     endsr

     *====================================================================*
     * WCOM - Write DARTS Remark
     *====================================================================*
     C     wcom          begsr

     C                   time                    dsdttm
     C                   eval      secLvl = 0
     C                   move      secLvl        psecl
     C                   move      'N'           fSecByLvl
    
     ** Load remark record fields
     C                   movel     eKey          pkey
     C                   clear                   pcsid
     C                   movel     ktpnam        ppnam
     C                   clear                   pcode
     C                   movel     dtrmcd        prmcd
     C                   movel     '***IMAGE'    pdcid
     C                   move      '*** '        pdcid
     C                   movel     dtdesc        pcomm
     C                   move      ' '           pfl01
     C                   movel     path          pfold
    
     C                   move      dsDate        pcdat
    
     C                   movel     'Y'           pimgf
    
     C***** Beg Add *** 09/09/15 **************************************
      /free

        // update Remark Comment to have new remark code from import
        // file if that is present

        //*** Begin Add *** 03/30/16 **********************************
        qqDcID = eDocId;
        //*** End   Add *** 03/30/16 **********************************
        Chain(n) qqdcid pop01;
        if %found(optimptl1);
           if OPcomm <> *blanks;
              pcomm = OPcomm;
           endif;
        endif;

      /end-free
     C***** End Add *** 09/09/15 **************************************
    
     ** Maintenance fields
     C                   movel     q1pgm         ppgm
     C                   movel     q1user        puser
     C                   move      'Y'           pdifl                                 DFT di FLag
     C                   move      'N'           pfl02
     C                   movel     q1pgm         pipgm
     ***** add 09/20/13 ***************************
     ***get diary flag
     C     CTKY          CHAIN     PCT01                              99
     C     *IN99         IFEQ      *OFF
     C     CTSTAT        ANDEQ     'A'
     C                   MOVEL(P)  CTFL01        pdifl
     C                   ENDIF
     ***** end add 09/20/13 **********************

     C                   callp     WriteRmk(pkey:psequence:pcomm:pcdat:prmcd:
     C                             pipgm:puser:ppnam:pdifl:pfl01:pfl02:
     C                             fSecByLvl:psecl:pdcid:pfold:pcom2:pImgf:
     C                             eDocID)

     C                   move      psequence     eSequence
    
     C     endwcom       endsr
    
     *===================================================================
     * BNOTES - CREATE BATCH NOTE
     *===================================================================
     C     bnotes        begsr
    
     C                   eval      noteSeq = *hival
     C     dnky1         setgt     pdn01
     C     dnky2         readpe    pdn01                                  60
     C                   if        *in60 = *off
     C                   eval      noteSeq = 1 + dnasq#
     C                   else
     C                   eval      noteSeq = 1
     C                   endif
    
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****              movel     dodcid        dndcid
     C                   movel     qqdcid        dndcid
     C***** END  CHG ***  10/17/11  **********************************
     C                   clear                   dncm75
     C                   move      noteSeq       dnasq#
     c                   callp     GetCurDate(xDate:xTime)
     c     *iso          move      xDate         dncdat
     c     *iso          move      xDate         dntrdt
     c     *hms          move      xTime         dntrtm
    
     C                   movel     q1pgm         dnpnam

     ** These security values are not currently used
     C                   clear                   dnsecl
     C                   clear                   dnsbyl

     ** Protect this record, as it is an 'H' History record
     C                   movel     'H'           dnprfl
    
     C                   clear                   dnstat
     C                   movel     q1user        dniusr
     C                   movel     q1pgm         dnipgm
     c     *ymd          move      xDate         dnidt
     c     *hms          move      xTime         dnitim

     C                   clear                   dnlusr
     C                   clear                   dnlpgm
     C                   clear                   dnltim
     C                   clear                   dnldt
    
     ** Format the comment
     C                   clear                   dncm75
     ** Line one - Batch History:
     c                   eval      dncm75 = fc(1)
     C                   write     pdn01
    
     ** Line two - Status:
     C                   eval      noteSeq = noteSeq + 1
    
     C                   clear                   dncm75
     C                   move      noteSeq       dnasq#
    
     ** Get the batch status table record for the description
     C     dsky1         chain     pds01                              61
     c                   eval      dncm75 = fc(2) + batchSts + ' - ' + dsldsc
     C                   write     pdn01
    
     ** Line three - Date/Time:
     C                   eval      noteSeq = noteSeq + 1
    
     C                   clear                   dncm75
     C                   move      noteSeq       dnasq#
    
     c                   callp     GetCurDate(xDate:xTime)
     c     *usa          move      xDate         dateChar
     c     *usa          move      xTime         timeChar
     c                   eval      dncm75 = fc(3) + dateChar + '     ' +
     c                             timeChar
     C                   write     pdn01
    
     ** Line four - User:
     C                   eval      noteSeq = noteSeq + 1
    
     C                   clear                   dncm75
     C                   move      noteSeq       dnasq#
    
     c                   eval      dncm75 = fc(4) + q1user
     C                   write     pdn01

     C     endbnotes     endsr

     *===================================================================
     * SRVALIDX - Validate the index information & write DARTS
     *===================================================================
     C     srvalidx      begsr

     *  Verify the Index passed in.  If valid, create the DARTS record.
     *  If not, process like the high speed option.
    
     C     dtky2         chain     pdt01                              60
     C                   if        *in60 = *on
     C                   movel     '4'           errorCode
     C     errorCode     cabne     *blanks       endvalidx
     C                   endif
    
     C                   movel     dtidxp        indexPgm
    
     ** Get the verify program to call
     C     ktky1         chain     pkt01                              60
     C                   if        *in60 = *off
     *** Begin Change *** 09/27/00 ****************************************************************
     * This is for NDP's special situation where the actual key has a
     * different format than what the key type says.  They initally use
     * a dummy key when an image is scanned from Ascent because a
     * policy number has not been assigned yet.
     *
     C                   if        eRtnCode = 'Y'
     ** This is a special case so let's assume that the key is valid
     C                   eval      errorCode = *blank
     C                   else
     ** Go ahead and call the verify program, this isn't a special case
     C                   call      ktvpgm
     C                   parm                    eKey
     C                   parm      *blank        errorCode
     C                   endif
     *** End Change *** 09/27/00 ******************************************************************
    
     C                   if        errorCode <> *blank
     C                   clear                   errorCode
     C                   clear                   eKey
     **** add 9/20/13 ***************************
     C                   clear                   cimpf
     ***  end add 9/20/13 ************************
     C                   else
     C*** BEGIN ADD *** 04/05/11 **************************************
     C*
     C* DO WE RESTRICT AUTO INDEX?
     C*
     C     DTFL01        IFNE      'Y'
     C*
     C* AUTO INDEX
     C*
     C*** END   ADD *** 04/05/11 **************************************
     C                   exsr      wcom
     C*** BEGIN ADD *** 04/05/11 **************************************
     C                   ELSE
     C*
     C* DO NOT AUTO INDEX
     C*
     C                   EXSR      BARIDX
     C                   MOVEL     *BLANKS       ekey
     C                   MOVE      0             esequence
     C                   ENDIF
     C*** END   ADD *** 04/05/11 **************************************
     C                   endif
    
     C                   else
     C                   clear                   eKey
    
     C                   endif

     C     endvalidx     endsr

     *===================================================================
     * SRWRTDO - Write the DO records
     *===================================================================
     C     srwrtdo       begsr

     C***** BEG  CHG ***  10/17/11  **********************************
     C*****              movel     eDocId        dodcid
     C*****              movel     eImgFldr      doflr
     C*****              movel     eImgSfldr     dosflr
     C*****              clear                   doosys
     C*****              clear                   dolibn
     C*****              clear                   doofld
     C*****              clear                   doosfl
     C*****              clear                   doodci
     C*****              clear                   doarqs
     C*****              movel     eDocType      dodtyp
    
     C*****              move      eNumBytes     numBytes
     C*****              eval      dobytd = numBytes / 1000
    
     C*****              movel     eWsId         dowsid
     c*****              eval      xDate = CvtToDate(#ScanDate)
     c******iso          move      xDate         dosdt
     C*****              move      eScanTime     dostmd
     C*****              movel     q1user        dosrep
     C*****              movel     eBatchId      dobchi
     C*****              move      eNumPages     dopgs
     C*****              clear                   dooptf
    
     C*****              if        eKey <> *blanks
     C*****              movel     'Y'           doaifl
     C*****              movel     'Y'           doidfx
     C*****              else
     C*****              movel     'N'           doidfx
     C*****              clear                   doaifl
     C*****              endif
    
     C*****              clear                   dovol1
     C*****              clear                   dovol2
     C*****              clear                   dovol3
     C*****              movel     eKey          dodkey
     C*****              move      eSequence     dodsq#
     *** BEG ADD ********** 07/09/07 ********************************
     C*****              If        #RcvdDate <> 0
     c*****              Eval      xDate = CvtToDate(#RcvdDate)
     c******iso          Move      xDate         dorcvd
     C*****              Else
     C*****              Eval      dorcvd = dosdt
     C*****              EndIf
     *** END ADD ********** 07/09/07 ********************************
     C*****              clear                   dolpgm
     C*****              clear                   dolusr
     C*****              clear                   doldt
     C*****              clear                   doltim
    
     C*****              movel     q1user        doiusr
     C*****              movel     q1pgm         doipgm
     c*****              callp     GetCurDate(xDate:xTime)
     c*** Begin Chg *** 01/18/11 *******************************************
     C******hms*         move      xTime         doidt
     C******ymd*         move      xDate         doitim
     C******ymd          move      xDate         doidt
     C******hms          move      xTime         doitim
     c*** End   Chg *** 01/18/11 *******************************************
    
     C*****              if        doidfx = 'N'
     C*****              movel     'SC'          dodsts
     C*****              else
     C*****              movel     'CM'          dodsts
     C*****              movel     'CM'          batchSts
     C*****              endif
    
     C*****              write     pdo01
    
     C*****              if        eKey <> *blanks
     C*****              write     pdo011
     C*****              endif

     C                   movel     eDocId        qqdcid
     C                   movel     eImgFldr      qqflr
     C                   movel     eImgSfldr     qqsflr
     C                   clear                   qqosys
     C                   clear                   qqlbn
     C                   clear                   qqofld
     C                   clear                   qqosfl
     C                   clear                   qqodci
     C                   clear                   qqarqs
     C                   movel     eDocType      qqdtyp
    
     C                   move      eNumBytes     numBytes
     C                   eval      qqbytd = numBytes / 1000
    
     C                   movel     eWsId         qqwsid
     c                   eval      xDate = CvtToDate(#ScanDate)
     c     *iso          move      xDate         qqsdt
     C                   move      eScanTime     qqstmd
     C                   movel     q1user        qqsrep
     C                   movel     eBatchId      qqbchi
     C                   move      eNumPages     qqpgs
     C                   clear                   qqoptf
    
     C                   if        eKey <> *blanks
     C                   movel     'Y'           qqaifl
     C                   movel     'Y'           qqidfx
     C                   else
     C                   movel     'N'           qqidfx
     C                   clear                   qqaifl
     C                   endif
    
     C                   clear                   qqvol1
     C                   clear                   qqvol2
     C                   clear                   qqvol3
     C                   movel     eKey          qqdkey
     C                   move      eSequence     qqdsq#
     C                   If        #RcvdDate <> 0
     c                   Eval      xDate = CvtToDate(#RcvdDate)
     c     *iso          Move      xDate         qqrcvd
     C                   Else
     C                   Eval      qqrcvd = qqsdt
     C                   EndIf
    
     C                   if        qqidfx = 'N'
     C                   movel     'SC'          qqdsts
     C                   else
     C                   movel     'CM'          qqdsts
     C                   movel     'CM'          batchSts
     C                   endif
     C                   move      'Y'           qqindx
    
     C* Call API to write Document and Folder Info
     C                   call      'ISA700R'     isakey
     C***** END  CHG ***  10/17/11  **********************************

     C     endwrtdo      endsr

     *===================================================================
     * SRNOTIDX - What to do if not an index type
     *===================================================================
     C     srnotidx      begsr

     C***** BEG  CHG ***  10/17/11  **********************************
     C*****              movel     dodcid        iedcid
     C*****              movel     dodtyp        iedoct
     C                   movel     eDocId        iedcid
     C                   movel     eDocType      iedoct
     C***** END  CHG ***  10/17/11  **********************************
    
     C     dtky1         chain     pdt01                              60
     C                   if        *in60 = *off
     C                   movel     dtdque        ieqid
     C                   else
     C                   movel     '4'           errorCode
     C                   endif

     C     errorCode     cabne     *blanks       endnotidx
    
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****              move      dosdt         iesdt
     C*****              move      dostmd        iestm
     C                   move      qqsdt         iesdt
     C                   move      qqstmd        iestm
     C***** END  CHG ***  10/17/11  **********************************
     C                   movel     q1user        iesrep
     C                   movel     'A'           iestat
    
     C                   movel     q1user        ieiusr
     C                   movel     q1pgm         ieipgm
     c                   callp     GetCurDate(xDate:xTime)
     C     *hms          move      xTime         ieitim
     C     *ymd          move      xDate         ieidt
     C                   clear                   ielpgm
     C                   clear                   ielusr
     C                   clear                   ieldt
     C                   clear                   ieltim
    
     C***** BEG  CHG ***  10/17/11  **********************************
     C*****              movel     dodsts        iedsts
     *** BEG ADD ********** 07/09/07 ********************************
     C*****              move      dorcvd        iercvd
     *** END ADD ********** 07/09/07 ********************************
     **** add 9/20/13 ****************************
     C                   if        cimpf <> *blanks
     C                             and ekey <> *blanks
     C                   Movel     'CM'          iedsts
     C                   else
     **** end add 9/20/13 ****************************
     C                   movel     qqdsts        iedsts
     **** add 9/20/13 ****************************
     C                   endif
     **** end add 9/20/13 ****************************
     C                   move      qqrcvd        iercvd
     C***** END  CHG ***  10/17/11  **********************************
    
     C                   write     pie01

     C     endnotidx     endsr

     *===================================================================
     * SRUPDDI - Update DI records
     *===================================================================
     C     srupddi       begsr

     C     diky1         chain     pdi01                              60
    
     C                   if        *in60 = *off
    
     C                   move      eDocSeq       docSeq#
     C                   move      eDocSeq       docSeq2
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   if        diseq#=999999
     C                   eval      diseq#=-1
     C                   else
     C                   if        diseq#=-1
     C                   move      'S'           errorCode
     C                   else
     C*** END   ADD *** 05/30/08 **************************************
     C                   eval      diseq# = diseq# + 1
     C*** BEGIN ADD *** 05/30/08 **************************************
     C                   endif
     C                   endif
     C*** END   ADD *** 05/30/08 **************************************
    
     C                   movel     q1user        dilusr
     C                   movel     q1pgm         dilpgm
     c                   callp     GetCurDate(xDate:xTime)
     C     *hms          move      xTime         diltim
     C     *ymd          move      xDate         dildt
    
     C                   update    pdi01
    
     C                   else
     ** ERROR - Document Index record not found
     C                   move      '1'           errorCode
     C                   endif

     C     endupdi       endsr

     *===================================================================
     * SRFLDBAL - Folder balance routine
     *===================================================================
     C     srfldbal      begsr
     C     fbky1         chain     pfb01                              60
    
     C                   if        *in60 = *off
    
     C                   move      eNumDocs      #NumDocs
    
     ** Increment number of docs
     C                   eval      fbc#ds = fbc#ds + 1
    
     C                   if        fbc#ds >= fbm#ds
     C                   exsr      newsfl
    
     C                   if        errorCode <> *blanks
     C                   move      '3'           errorCode
     C     errorCode     cabne     *blanks       endfldbal
     C                   endif
    
     C                   endif
    
     C                   update    pfb01
    
     C                   else
     ** ERROR - Folder Balance record does not exist
     C                   move      '2'           errorCode
     C                   endif

     C     endfldbal     endsr
     *===================================================================




     *===================================================================
     * Table Entries
     *===================================================================
**   RMRK ENTRIES
Doc.  History:
   Mass Chg Status   :
            Date/Time:
            User     :
            Error    :
