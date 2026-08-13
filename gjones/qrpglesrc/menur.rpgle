      // @MBRTEXT: Training Menu
      /DEFINE PROFOUNDUI

     H DFTACTGRP(*NO)

     FMENUD     CF   E             WorkStn Handler('PROFOUNDUI(HANDLER)')

     D* External program is defined by menu response field PROGRAM, from
     D* the DDS.
     D CallProgram     PR                  ExtPgm(PROGRAM)

     D PSDS           SDS
     D  JobName              244    253A
     D  JobUser              254    263A
     D  JobNumber            264    269A

      /FREE

        JOB = JobNumber + '/' + %Trim(JobUser) + '/' + %Trim(JobName);

        DoW Not *In03;

          ExFmt MENUR1;
          If Not *In03;
            CallProgram();
          EndIf;

        EndDo;

        *InLr = *On;
        Return;

      /END-FREE
