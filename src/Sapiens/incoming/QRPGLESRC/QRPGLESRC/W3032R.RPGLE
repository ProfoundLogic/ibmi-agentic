     HCopyright('(c) 2025 Sapiens. All Rights Reserved.')
     h/title W3032R  - Agent commission statement resolve to DARTS.
     h option(*nodebugio: *srcstmt)
     h debug(*yes)
     h bnddir('WTBNDDIR')
     *=====================================================================
      * Called from: W3030C
      * Calls to:    W7421C
      * Created 02/06/12  C:255  Project: 114WBD
     *=====================================================================
      * Revision:
      *
      * 12/11/25 C:65  Project: Phw#120223 (Internal)
      *                Submit output to Scriptura after all statements
      *                have been generated.
      *
      * 11/04/14 C:279 Project: 129WTQ
      *                Changed to save modified statement to DARTS
      *
      * 02/25/14 C:65  Project: Redmine #4002 (v14r1 testing)
      *               -Pass zeroes in for the agent# to W7421C to allow
      *                queries to successfully run.   '00000' is considered
      *                no agent by that program.
      *
      * 01/08/14 C:136 Support: WA114 84453
      *               -Correct date parms for call to W7421R
      *               -Add mass run per GW
      *               -Changes not commented
     *=====================================================================
     fwmupl     if   e           k disk
     fwmlsl1    if   e           k disk
     *=====================================================================
      * Data Structures/Arrays
     *=====================================================================
     dpsds           esds                  extname(SZQ1P)
     *=====================================================================
      * Prototypes
     *=====================================================================
      ******* beg chg ****** 11/04/14 **************************************
     d******* RunAgtStmts     pr                  extpgm('W7421C')
     d RunAgtStmts     pr                  extpgm(strPgmNm)
      ******* end chg ****** 11/04/14 **************************************
     d  ##co#                         3    const
     d  ##fnd                         3    const
     d  ##frdt                        8    const
     d  ##todt                        8    const
     d  ##outq                       10    const
     d  ##agt#                        5    const
     d  ##emp#                        9    const
     d  ##div                         5    const
     d  ##zbf                         1    const
     d  ##typo                        1    const
     d  ##rmcd                        5    const
     d/copy sprDate
     *=====================================================================
      * Stand Alone Fields
     *=====================================================================
     d  xxCo#          s                   like(LSco#)
     d  xxFnd          s                   like(LSfnd)
     d  xxFrdt         s              8  0
     d  xxTodt         s              8  0
      ******* beg add ****** 11/04/14 **************************************
     d  strPgmNm       s             10    inz

     d*** begin chg ***  12/11/25  ********************************
     d*lda*******      ds            10    dtaara(*LDA)
     d**ldPgmNm**              1     10
     d lda             ds          1024    dtaara(*LDA)
     d  ldPgmNm                1     10
     D ldMassFile                    60    Overlay(lda:129)
     d
     d S4401C          pr                  ExtPgm('S4401C')
     d   DTASRC_                     61    const
     d
     d  xmlFlowName    s             60    inz(' ')
     d
     DObjExists        pr             1n
     D object                        10    const
     D objType                        7    const
     d*** end chg *****  12/11/25  ********************************
      ******* end add ****** 11/04/14 **************************************
     *=====================================================================
      * Plists
     *=====================================================================
     c     *entry        plist
     c                   parm                    qqCo#             3
     c                   parm                    qqFnd             3
     c                   parm                    qqFrdt            8
     c                   parm                    qqTodt            8
     c                   parm                    qqRmcd            5
     *=====================================================================
      * Main
     *=====================================================================
      /free
       Chain Q1cusr pup01;
       xxCo#  = %int(qqCo#);
       xxFnd  = %int(qqFnd);
       xxFrdt = DtCYMD(%int(qqFrdt));
       xxTodt = DtCYMD(%int(qqTodt));
       //****** beg add ********* 11/04/14 *****************************************
       In *dtaara;
       strPgmNm = ldPgmNm;
       Out *dtaara;
       //****** end add ********* 11/04/14 *****************************************
       // Run mass statements
       Chain (xxCo#:xxFnd:xxFrdt:xxTodt) pls01;
       //****** beg add ********* 11/04/14 *****************************************
       Monitor;
       //****** end add ********* 11/04/14 *****************************************
       RunAgtStmts(qqCo#:
                   qqFnd:
                   qqFrdt:
                   qqTodt:
                   UPoutq:
                   //*** begin chg ***  02/25/14  **********************************
                   //********blanks:
                   '00000':
                   //*** begin chg ***  02/25/14  **********************************
                   *zeros:
                   *zeros:
                   LSzbf:
                   LStypo:
                   qqRmcd);
       //****** beg add ********* 11/04/14 *****************************************
       on-Error;
       EndMon;
       //****** end add ********* 11/04/14 *****************************************
       // Run single agent statements
       Setll (xxCo#:xxFnd:xxFrdt:xxTodt) pls01;
       Reade (xxCo#:xxFnd:xxFrdt:xxTodt) pls01;
       Dow not(%eof);
          //****** beg add ********* 11/04/14 *****************************************
          Monitor;
          //****** end add ********* 11/04/14 *****************************************
          RunAgtStmts(qqCo#:
                      qqFnd:
                      qqFrdt:
                      qqTodt:
                      UPoutq:
                      %editc(LSagt#:'X'):
                      *zeros:
                      *zeros:
                      LSzbf:
                      'P':
                      qqRmcd);
          //****** beg add ********* 11/04/14 *****************************************
          on-Error;
          EndMon;
          //****** end add ********* 11/04/14 *****************************************
       Reade (xxCo#:xxFnd:xxFrdt:xxTodt) pls01;
       Enddo;
       //*** begin add ***  12/11/25  ********************************
       In *dtaara;
       Out *dtaara;
       If ldMassFile <> ' ';
          xmlFlowName = ldMassFile;
          Select;
          When objExists('PDSDEV':'*LIB');
             Monitor;
             S4401C(xmlFlowName + '$');
             On-error;
             Endmon;
          Other;
             S4401C(xmlFlowName + '$');
          Endsl;
       Endif;
       //*** end add *****  12/11/25  ********************************
       *inlr = *on;
      /end-free
     C*** begin add ***  12/11/25  ********************************
     P*============================================
     P* Determine if the object exists
     P*============================================
     PObjExists        b
     DObjExists        pi             1n
     D object                        10    const
     D objType                        7    const

     D obj             s             10
     D type            s              7
     D error           s              1n

     C                   call      'SRCHKOBJ'
     C                   parm      object        obj
     C                   parm      objType       type
     C                   parm                    error

     C                   return    not error

     PObjExists        e
     C*** end add *****  12/11/25  ********************************
