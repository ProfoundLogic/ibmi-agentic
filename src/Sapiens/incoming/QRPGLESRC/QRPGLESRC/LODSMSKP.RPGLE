     HCopyright('(c) 2022 Sapiens. All Rights Reserved.')
     H dftactgrp(*no) actgrp(*caller)
     h option(*nodebugio: *srcstmt)
     h bnddir('WTBNDDIR':'QC2LE')
     H/TITLE LODSMSKP - Load Key Information
     H*=====================================================================
     H* Created: 09/21/22 Project: WC2838  C: 291
     H*
     H* Note: This program will run with release load. To test
     H* Encrypt/Decrypt in any test Environment, need to run this program
     H* manually from command line to generate a key.
     H*=====================================================================
     H* Revisions:
     H*
     H*=====================================================================

      * Secured Key File
     Fsmskp     uf a e             disk    usropn Rename(psk01:psk00)

      * System includes
     d/Copy QSYSINC/QRPGLESRC,QUSEC
     d/Copy QSYSINC/QRPGLESRC,QC3CCI
     d/copy sPrDate

      * Prototypes
     DGenSymKey        pr                  extproc('Qc3GenSymmetricKey')
     D keyType                       10i 0 const
     D keySize                       10i 0 const
     D keyFormat                      1    const
     D keyForm                        1    const
     D KEKKey                         1    const
     D KEKAlg                         8    const
     D csp                            1    const
     D cspDevNam                     10    const options(*omit)
     D KeyString                      1
     D KeyStringLen                  10i 0 const
     D KeyLenRtn                     10i 0
     D errCod                         1

     d psds          esds                  extname(szq1p)

      * Local variable
     D csp             s              1    inz('0')
     D AESctx          s              8
     D KEKctx          s              8
     D keyType         s             10i 0
     D keyLen          s             10i 0
     D keyFormat       s              1
     D keyForm         s              1

      /Free
       // Generate a file key encrypted under SYSDTAKEK
       keyType = 22;                  //22 = AES
       keyFormat = '0';               //0 = Binary String
       keyForm = '0';                 //0 = Clear. The key string is returned in the clear
       KEKctx = *Blanks;              //For key form 0 (clear), this parameter must be set to blanks
       AESctx = *Blanks;              //For key form 0, key encrypting algorithm must be blanks

       GenSymKey(keyType:32:keyFormat:keyForm:KEKctx:AESctx:csp:*OMIT:SKKey:32:
        keyLen:QUSEC);

       // Write record with encrypted key file key to SMSKP
       If not %Open(SMSKP);
          Open SMSKP;
       Endif;
       SKuser = q1user;
       SKpgm  = q1pgm;
       SKtime = Timeto6(SystemTime());
       SKdt   = Dateto7(SystemDate());
       SKiusr = q1user;
       SKipgm = q1pgm;
       SKitim = Timeto6(SystemTime());
       SKidt  = Dateto7(SystemDate());
       Write(e) psk00;

       If %Open(SMSKP);
          Close SMSKP;
       Endif;
       *Inlr = *on;
      /End-Free

