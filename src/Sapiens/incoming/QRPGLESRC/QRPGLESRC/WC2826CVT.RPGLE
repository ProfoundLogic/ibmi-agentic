     HCopyright('(c) 2024 Sapiens. All Rights Reserved.')
     H option(*nodebugio:*srcstmt)
     H dftactgrp(*no) actgrp('QILE')
     H BNDDIR('WTBNDDIR')
     H*****************************************************************
     H* Program Description:  This program perform below tasks         *
     H*      - Convert Agent contact table (old values from file WTAYP *
     H*        to new file STCNP.                                      *
     H*      - Add initial claim contact values to new file STCNP.     *
     H*                                                                *
     H*   Created   : 04/02/24                                         *
     H*   Programmer: C:333                                            *
     H*   Project   : WC2826 - Claims Contacts.                        *
     H*                                                                *
     H* THE TYPE TABLE CONTAINS CONTACT TYPES.                         *
     H*                                                                *
     H* THE DES TABLE CONTAINS DESCRIPTION.                            *
     H*                                                                *
     H* ===============================================================*
     H*   Revisions:                                                   *
     H*                                                                *
     H*                                                                *
     H* ===============================================================*
     H*****************************************************************

      *System company file for work comp
     fsmcol     if   e           k disk    rename(pco01:pco00)

      *Agent Contact Type
     fwtayl1    if   e           k disk    rename(pay01:pay00)

      *Contact Type File
     fstcnl     uf a e           k disk    rename(pcn01:pcn0l)

     d/copy sPrDate
     d/copy sPrString
     d psds          esds                  extname(SZQ1P)
     d size            c                   50
     d i               s              5  0 inz
     d kkco#           s              3  0
     d ctype           s             30    dim(size) ctdata perrcd(1)
     d ptype           s             30    dim(size) ctdata perrcd(1)
     d cdes            s             40    dim(size) ctdata perrcd(1)
     d pdes            s             40    dim(size) ctdata perrcd(1)
     d AdjHdg          s             20
     d LossCtlHdg      s             30
     d xxHeading       s               n   inz(*Off)

      /free

        Exsr CopyWtayp;

        Setll *loval pco00;
        Read pco00;
        If %eof;
           COco# = 777;
        Endif;

        Dou %eof;
           kkCO# = COco#;
           Exsr ProcessCo;
           Read pco00;
        Enddo;

       *inlr = *on;

       //===================================================================
       // Copying WTAYP records to STCNP
       //===================================================================
       Begsr CopyWtayp;

        Setll *loval pay00;
        Read pay00;
        Dou %eof;
           Chain (AYco#:AYtype) pcn0l;
           If %found(STCNL);
              Exsr UpdstcnpRec1;             // Update Agent type record
           Else;
              Exsr WrtstcnpRec1;             // Write Agent type record
           Endif;
           Read pay00;
        Enddo;

       Endsr;

       //===================================================================
       // Adding claims records to STCNP
       //===================================================================
       Begsr ProcessCo;

        //************** CLAIM TYPE ************************//

        //Read thru **CTYPE compile time data area
        For i = 1 to size;
          If ctype(i) = *blanks;
             Iter;
          Endif;

        //If STCNP record doesn't exist, write record else update
          Chain (kkCO#:ctype(i)) pcn0l;
          If not %found(STCNL);
            Exsr WrtstcnpRec2;             // Write Claim type record
          Else;
            Exsr UpdstcnpRec2;             // Update Claim type record
          Endif;
        Endfor;

        Exec sql DECLARE mycursor1 Cursor For   // Internal Claims Contacts

         Select DISTINCT CXadj
          From WMCXP
          Where CXfnd IN (
           Select CMfnd From WMCMP
            Where CMstat <> 'I' );

        Exec sql OPEN mycursor1;

        Exec sql FETCH NEXT from mycursor1
          INTO :AdjHdg;

        Dow (sqlcod = 0);
           xxHeading = *on;

           If AdjHdg = ' ';
              AdjHdg = 'Adjuster';
           Endif;

           Chain (kkCO#:Upper(AdjHdg)) pcn0l;
           If %found(STCNL);
              Exsr UpdstcnpRec2;             // Update Claim Contact record
           Else;
              Exsr WrtstcnpRec2;             // Write Claim Contact record
           Endif;

           Clear AdjHdg;
           xxHeading = *off;

           Exec sql FETCH NEXT from mycursor1
             INTO :AdjHdg;

        Enddo;
        Exec sql CLOSE mycursor1;

        //********************** Policy TYPE ****************************//

        //Read thru **PTYPE compile time data area
        For i = 1 to size;
          If ptype(i) = *blanks;
             Iter;
          Endif;

        //If STCNP record doesn't exist, write record else update
          Chain (kkCO#:ptype(i)) pcn0l;
          If not %found(STCNL);
            Exsr WrtstcnpRec3;             // Write Policy type record
          Else;
            Exsr UpdstcnpRec3;             // Update Policy type record
          Endif;
        Endfor;

        Exec sql DECLARE mycursor2 Cursor For   // Internal Policy Contacts

         Select DISTINCT EXahlc
          From WMEXP
          Where EXfnd IN (
           Select CMfnd From WMCMP
            Where CMstat <> 'I' );

        Exec sql OPEN mycursor2;

        Exec sql FETCH NEXT from mycursor2
          INTO :LossCtlHdg;

        Dow (sqlcod = 0);
           xxHeading = *on;

           If LossCtlHdg = ' ';
              LossCtlHdg = 'Loss Control Representative';
           Endif;

           Chain (kkCO#:Upper(LossCtlHdg)) pcn0l;
           If %found(STCNL);
              Exsr UpdstcnpRec3;           // Update Policy Contact record
           Else;
              Exsr WrtstcnpRec3;           // Write Policy Contact record
           Endif;

           Clear LossCtlHdg;
           xxHeading = *off;

           Exec sql FETCH NEXT from mycursor2
             INTO :LossCtlHdg;

        Enddo;
        Exec sql CLOSE mycursor2;

       Endsr;

       //===================================================================
       // Write records to STCNP for Context = 'Agent'
       //===================================================================
       Begsr WrtstcnpRec1;

        Clear pcn0l;
        CNco# =  AYco#;
        CNtype = AYtype;
        CNdesc = AYdesc;
        CNctxt = 'Agent';
        CNsys = ' ';
        CNstat = AYstat;
        CNuser = Q1user;
        CNpgm = Q1pgm;
        CNtime = Timeto6(SystemTime());
        CNdt   = Dateto7(SystemDate());
        CNiusr = AYuser;
        CNipgm = AYpgm;
        CNitim = AYtime;
        CNidt  = AYdt;

        Write pcn0l;

       Endsr;

       //===================================================================
       // Update records to STCNP for Context = 'Agent'
       //===================================================================
       Begsr UpdstcnpRec1;

        CNco# =  AYco#;
        CNtype = AYtype;
        CNdesc = AYdesc;
        CNctxt = 'Agent';
        CNsys = ' ';
        CNstat = AYstat;
        CNuser = Q1user;
        CNpgm = Q1pgm;
        CNtime = Timeto6(SystemTime());
        CNdt   = Dateto7(SystemDate());

        Update pcn0l;

       Endsr;

       //===================================================================
       // Write records to STCNP for Context = 'Claims'
       //===================================================================
       Begsr WrtstcnpRec2;

        Clear pcn0l;
        CNco# =  kkCO#;

        If xxHeading = *Off;
           CNtype = ctype(i);
           CNdesc = cdes(i);
        Else;
           CNtype = Upper(AdjHdg);
           CNdesc = AdjHdg;
        Endif;

        CNctxt = 'Claims';
        CNsys = 'Y';
        CNstat = 'A';
        CNuser = Q1user;
        CNpgm = Q1pgm;
        CNtime = Timeto6(SystemTime());
        CNdt   = Dateto7(SystemDate());
        CNiusr = Q1user;
        CNipgm = Q1pgm;
        CNitim = Timeto6(SystemTime());
        CNidt  = Dateto7(SystemDate());

        Write pcn0l;

       Endsr;

       //===================================================================
       // Update records to STCNP for Context = 'Claims'
       //===================================================================
       Begsr UpdstcnpRec2;

        CNco# =  kkCO#;

        If xxHeading = *Off;
           CNtype = ctype(i);
           CNdesc = cdes(i);
        Else;
           CNtype = Upper(AdjHdg);
           CNdesc = AdjHdg;
        Endif;

        CNctxt = 'Claims';
        CNsys = 'Y';
        CNstat = 'A';
        CNuser = Q1user;
        CNpgm = Q1pgm;
        CNtime = Timeto6(SystemTime());
        CNdt   = Dateto7(SystemDate());

        Update pcn0l;

       Endsr;

       //===================================================================
       // Write records to STCNP for Context = 'Policy'
       //===================================================================
       Begsr WrtstcnpRec3;

        Clear pcn0l;
        CNco# =  kkCO#;

        If xxHeading = *Off;
           CNtype = ptype(i);
           CNdesc = pdes(i);
        Else;
           CNtype = Upper(LossCtlHdg);
           CNdesc = LossCtlHdg;
        Endif;

        CNctxt = 'Policy';
        CNsys = 'Y';
        CNstat = 'A';
        CNuser = Q1user;
        CNpgm = Q1pgm;
        CNtime = Timeto6(SystemTime());
        CNdt   = Dateto7(SystemDate());
        CNiusr = Q1user;
        CNipgm = Q1pgm;
        CNitim = Timeto6(SystemTime());
        CNidt  = Dateto7(SystemDate());

        Write pcn0l;

       Endsr;

       //===================================================================
       // Update records to STCNP for Context = 'Policy'
       //===================================================================
       Begsr UpdstcnpRec3;

        CNco# =  kkCO#;

        If xxHeading = *Off;
           CNtype = ptype(i);
           CNdesc = pdes(i);
        Else;
           CNtype = Upper(LossCtlHdg);
           CNdesc = LossCtlHdg;
        Endif;

        CNctxt = 'Policy';
        CNsys = 'Y';
        CNstat = 'A';
        CNuser = Q1user;
        CNpgm = Q1pgm;
        CNtime = Timeto6(SystemTime());
        CNdt   = Dateto7(SystemDate());

        Update pcn0l;

       Endsr;
       //===================================================================
      /end-free
      *
** CTYPE
CLAIMANT
TREATING PHYSICIAN
RESPONSIBLE PARTY
INSURANCE COMPANY
DEFENSE ATTORNEY
PLAINTIFF ATTORNEY
ADDITIONAL PAYEE
ACCOUNT MANAGER
CASE NURSE
CLAIM MANAGER
OUTSIDE ADJUSTER
REINSURANCE ADJUSTER
SIDF ADJUSTER
SUPERVISOR
** PTYPE
POSITION COLUMN
POLICY SERVICES
AUDIT REVIEWER
CLAIM CONTACT
UNDERWRITER
AUDIT MANAGER
PREMIUM ACCOUNTANT
** CDES
Claimant
Treating Physician
At Fault Party
Subrogation Company
Defense Attorney
Plaintiff Attorney
Additional Payee
Account Manager
Case Nurse
Claim Manager
Outside Adjuster
Reinsurance Adjuster
Special Injury/Disability Fund Adjuster
Supervisor
** PDES
Position column
Policy Services
Audit Reviewer
Claim Contact
Underwriter
Audit Manager
Premium Accountant
