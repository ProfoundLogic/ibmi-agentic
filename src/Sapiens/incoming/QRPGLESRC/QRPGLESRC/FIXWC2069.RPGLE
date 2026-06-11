     Hcopyright('(c) 2016 StoneRiver, Inc. All rights reserved. -
     H StoneRiver is a trademark of StoneRiver, Inc.')
    h*********************************************************************
    h/TITLE FIXWC2069 - Populate written & change amts from WDELP to WDEHP
     H option(*nodebugio: *srcstmt)
     H debug(*yes)
    h*********************************************************************
    h* Called from:                                                      *
    h*   Command line                                                    *
    h*                                                                   *
    h* Calls to:                                                         *
    h*   None                                                            *
    h*                                                                   *
    h* Created:  05/17/16     C:242    PROJECT: WC2069                   *
    h*********************************************************************
    h* Revisions:                                                        *
    h*                                                                   *
     h* xx/xx/xx C:xxx xxxxxxx xxxxx xxxxx                                *
    h*********************************************************************
     fwdelp     if   e           k disk
     fwdehl10   uf   e           k disk

     d updateWDEHP     s              1n
    d*=============================================================================================
    d* Procedure Prototypes
    d*=============================================================================================
     d/copy sprdate

      /free
       Read pel01;
       Dow not %eof(WDELP);
          Setgt (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) peh01;
          Readpe (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) peh01;
          Dow not %eof(WDEHL10);
             IF EHFAF <> 'X';
                updateWDEHP = *off;
                //Set WDEHP with WDELP values
                If EL1AAM <> 0 and EHW1as <> 0;
                   EHW1AS = EL1AAM;
                   EHX1AS = EL1AAM;
                   EHI1AS = EL1AAM;
                   EHJ1AS = EL1AAM;
                   updateWDEHP = *on;
                Endif;
                If EL2AAM <> 0 and EHW2as <> 0;
                   EHW2AS = EL2AAM;
                   EHX2AS = EL2AAM;
                   EHI2AS = EL2AAM;
                   EHJ2AS = EL2AAM;
                   updateWDEHP = *on;
                Endif;
                If EL3AAM <> 0 and EHW3as <> 0;
                   EHW3AS = EL3AAM;
                   EHX3AS = EL3AAM;
                   EHI3AS = EL3AAM;
                   EHJ3AS = EL3AAM;
                   updateWDEHP = *on;
                Endif;
                If EL4AAM <> 0 and EHW4as <> 0;
                   EHW4AS = EL4AAM;
                   EHX4AS = EL4AAM;
                   EHI4AS = EL4AAM;
                   EHJ4AS = EL4AAM;
                   updateWDEHP = *on;
                Endif;
                If EL5AAM <> 0 and EHW5as <> 0;
                   EHW5AS = EL5AAM;
                   EHX5AS = EL5AAM;
                   EHI5AS = EL5AAM;
                   EHJ5AS = EL5AAM;
                   updateWDEHP = *on;
                Endif;
                If EL6AAM <> 0 and EHW6as <> 0;
                   EHW6AS = EL6AAM;
                   EHX6AS = EL6AAM;
                   EHI6AS = EL6AAM;
                   EHJ6AS = EL6AAM;
                   updateWDEHP = *on;
                Endif;
                If EL7AAM <> 0 and EHW7as <> 0;
                   EHW7AS = EL7AAM;
                   EHX7AS = EL7AAM;
                   EHI7AS = EL7AAM;
                   EHJ7AS = EL7AAM;
                   updateWDEHP = *on;
                Endif;
                If ELSFAM <> 0 and EHWSF  <> 0;
                   EHWSF  = ELSFAM;
                   EHXSF  = ELSFAM;
                   EHISF  = ELSFAM;
                   EHJSF  = ELSFAM;
                   updateWDEHP = *on;
                Endif;
                If ELMFAM <> 0 and EHWMF  <> 0;
                   EHWMF  = ELMFAM;
                   EHXMF  = ELMFAM;
                   EHIMF  = ELMFAM;
                   EHJMF  = ELMFAM;
                   updateWDEHP = *on;
                Endif;
                IF updateWDEHP;
                   update peh01;
                Endif;
                Leave;
             Endif;
             Readpe (ELco#:ELfnd:ELfyr:ELemp#:ELdiv) peh01;
          EndDo;

          Read pel01;
       Enddo;

       *inlr = *on;
      /end-free
