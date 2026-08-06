--  GT Warehouse Mobile -- journals and journal receivers
--
--  Runs FIRST, before any table exists, so no table is ever created unjournaled.
--  Two separate journals by design:
--
--    GTJRN     transactional -- every business table. Before-images are captured
--              so that ROLLBACK works and so the Supervisor View can read an
--              audit trail straight out of the journal.
--    GTJRNIMG  images only -- GTIMAGE holds JPEG BLOBs, and journalling those
--              into the transactional receiver would swamp the audit trail we
--              actually want to show.
--
--  Journalling is not optional here. TIGERPOC is a plain library (CRTLIB, not
--  CREATE SCHEMA), so it has no QSQJRN and DB2 refuses INSERT/UPDATE/DELETE on
--  an unjournaled table under commitment control -- SQL7008.
--
--  codermake has no journal target type, so this is declared as a .table.sql
--  and produces a *FILE stamp. The objects it really creates are *JRNRCV/*JRN.
--
--  ------------------------------------------------------------------------
--  LIBRARY NAME: the one place in this project where a library is named.
--  CL commands issued through QCMDEXC must qualify their objects, and neither
--  CURRENT SCHEMA (it resolves to *LIBL) nor codermake's $LIBRARY substitution
--  (message files and binding directories only) is available inside a .sql.
--  To deploy into a different library, change the DEFAULT below and nothing
--  else -- all RPG, CL and DDS source resolves through *LIBL.
--  ------------------------------------------------------------------------

BEGIN
    DECLARE v_lib VARCHAR(10) DEFAULT 'TIGERPOC';
    DECLARE v_cnt INTEGER DEFAULT 0;

    --  Creates are wrapped in their own scope so that "already exists" on a
    --  rebuild is tolerated. The verification below sits OUTSIDE that scope,
    --  so a genuine failure still fails the build rather than being swallowed.
    BEGIN
        DECLARE CONTINUE HANDLER FOR SQLEXCEPTION BEGIN END;

        CALL QSYS2.QCMDEXC('CRTJRNRCV JRNRCV(' || v_lib || '/GTRCV0001) ' ||
                           'TEXT(''GT WMS transactional journal receiver'')');

        CALL QSYS2.QCMDEXC('CRTJRN JRN(' || v_lib || '/GTJRN) ' ||
                           'JRNRCV(' || v_lib || '/GTRCV0001) ' ||
                           'MNGRCV(*SYSTEM) DLTRCV(*YES) ' ||
                           'TEXT(''GT WMS transactional journal'')');

        CALL QSYS2.QCMDEXC('CRTJRNRCV JRNRCV(' || v_lib || '/GTRCVI001) ' ||
                           'TEXT(''GT WMS image journal receiver'')');

        CALL QSYS2.QCMDEXC('CRTJRN JRN(' || v_lib || '/GTJRNIMG) ' ||
                           'JRNRCV(' || v_lib || '/GTRCVI001) ' ||
                           'MNGRCV(*SYSTEM) DLTRCV(*YES) ' ||
                           'TEXT(''GT WMS image journal'')');
    END;

    --  Both journals must exist or the whole build is pointless -- every table
    --  created after this point depends on one of them.
    SELECT COUNT(*) INTO v_cnt
      FROM TABLE(QSYS2.OBJECT_STATISTICS(v_lib, '*JRN')) AS J
     WHERE J.OBJNAME IN ('GTJRN', 'GTJRNIMG');

    IF v_cnt < 2 THEN
        SIGNAL SQLSTATE '75002'
           SET MESSAGE_TEXT = 'GTJRN and/or GTJRNIMG were not created';
    END IF;
END
