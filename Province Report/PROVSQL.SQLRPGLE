000100190313       //**************************************************************************
000200191105       //* THIS PROGRAM USES A CURSOR TO LOAD A TEMPORARY RESULT TABLE FROM 3
000300191105       //* SEPARATE TABLES, ONTARIO, QUEBEC AND MANITOBA. A NUMBER IS PASSED
000400191105       //* TO THE PROGRAM TO DETERMINE WHICH RECORDS ARE INLCUDED FROM THE 3 TABLES
000600190313       //**************************************************************************
000700190313           DCL-F PROVREPORT PRINTER OFLIND(*IN01) ;
000800190313           DCL-S ProvinceH    Char(10);
000900190313           DCL-S EndOfFile    IND;
001000190313           DCL-S TotalRecords PACKED(5:0) ;
001100190315             // LowLimit is passed to the program
001200191121           DCL-PI MAIN EXTPGM('PROVSQL');
001201191121             LOWLIMITIN PACKED(15:5);
001202191121           END-PI;
001300190315
001400190315
001401190315             // All Host Variables available under a single name
001500191121           DCL-DS CUSTOMERRECORD;
001501191121             CUSTID CHAR(6);
001502191121             FULLNAME CHAR(31);
001503191121             CITY CHAR(20);
001504191121             PURCHASE PACKED(7:2);
001505191121             PDATE DATE;
001506191121             PROVINCE CHAR(10);
002200191121          End-Ds CUSTOMERRECORD;
002300070123     C/EJECT
002400070123     C**************************************************************************
002500070123     C*                        ***   M A I N   R O U T I N E   ***
002600070123     C**************************************************************************
002700070118      /FREE
002701191121                LOWLIMIT = LOWLIMITIN;
002800100309                EXSR PrepareFiles;
002900190313                Write Title;
003000190313                Write HEADINGS;
003100100309                EXSR GetRow;
003200190313                Write NewProv;
003300070209                ProvinceH = Province;
003400070202                DOW NOT EndOfFile;
003500190313                    IF *IN01 = *ON;
003600190313                        Write Title;
003700190313                        Write HEADINGS;
003800190313                       *IN01 = *OFF;
003900070209                    ENDIF;
003901190315                    // Province may change
003902190315
003903191121                    IF PROVINCE = PROVINCEH;
003904191121                      WRITE DETAIL;
003905191121                    ELSE;
003906191121                      PROVINCEH = PROVINCE;
003907191121                      WRITE TOTALS;
003908191121                      TOTALPURCH = 0;
003909191121                      WRITE NEWPROV;
003910191121                      WRITE DETAIL;
003911191121                    ENDIF;
003912191121
004900190313                    TotalPurch     = TotalPurch     + Purchase;
005000070202                    TotalRecords= TotalRecords + 1;
005100100309                    EXSR GetRow;
005200070119                ENDDO;
005300190313                Write Totals;
005400070202                EXSR   WRAPUP;
005500190313                Write UnderLimit;
005600070119                *INLR = *ON;
005700070119                RETURN;
005800100309        //**********************************************************************
005900100309        // O P E N F I L E S   S U B R O U T I N E
006000100309        //**********************************************************************
006100100309         BEGSR  PrepareFiles;
006200190315        // S E T   U P   T H E   T E M P O R A R Y   R E S U L T  STRUCTURE
006300190315
006301191121         EXEC SQL
006302191121           DECLARE ALLPROVCURSOR CURSOR
006303191121             FOR
006304191121             SELECT CUSTID, TRIM(FNAME) || ' ' || LNAME, CITY, PURCHASE,
006306191121                        PDATE, 'ONTARIO' AS PROVINCE
006307191121             FROM BCI433LIB/ONTARIO
006308191121             WHERE PURCHASE > :LOWLIMIT
006309191121             UNION ALL
006310191121             SELECT CUSTID, TRIM(FNAME) || ' ' || LNAME, CITY, PURCHASE,
006311191121                        PDATE, 'QUEBEC' AS PROVINCE
006312191121             FROM BCI433LIB/QUEBEC
006313191121             WHERE PURCHASE > :LOWLIMIT
006314191121             UNION ALL
006315191121             SELECT CUSTID, TRIM(FNAME) || ' ' || LNAME, CITY, PURCHASE,
006316191121                        PDATE, 'MANITOBA' AS PROVINCE
006317191121             FROM BCI433LIB/MANITOBA
006318191121             WHERE PURCHASE > :LOWLIMIT;
006344190315
008500100309        // A   T E M P O R A R Y   R E S U L T   T A B L E   I S   C R E A T E D
009300191121         EXEC SQL
009301191121           OPEN ALLPROVCURSOR;
009302191121
009303191121         IF SQLCODE <> 0 OR SQLWN0 = 'W';
009304191121           ENDOFFILE = *ON;
009305191121         ENDIF;
009306190315
009400100309            ENDSR;
009500100309        //**********************************************************************
009600100309        //   G E T     R O W    S U B R O U T I N E
009700100309        //**********************************************************************
009800100309            BEGSR     GETROW;
010800100309
010801191121            EXEC SQL
010802191121              FETCH NEXT
010803191121                FROM ALLPROVCURSOR
010804191121                INTO :CUSTOMERRECORD;
010805190315
010806191121            IF SQLCODE <> 0 OR SQLWN0 = 'W';
010807191121               ENDOFFILE = *ON;
010808191121            ENDIF;
010814190315
010900191121            ENDSR;
011000100309        //**********************************************************************
011100100309        // W R A P U P     S U B R O U T I N E
011200100309        //**********************************************************************
011300100309           BEGSR WRAPUP;
011301191121
011302191121             EXEC SQL
011303191121               CLOSE ALLPROVCURSOR;
011304191121
011305191121             IF SQLCODE <> 0 OR SQLWN0 = 'W';
011306191121               ENDOFFILE = *ON;
011307191121             ENDIF;
011308191121
011311191121             EXEC SQL
011312191121               SELECT COUNT(*) INTO :ONTTOTAL
011313191121               FROM BCI433LIB/ONTARIO
011314191122               WHERE PURCHASE <= :LOWLIMIT;
011315191121
011316191121             IF SQLCODE <> 0 OR SQLWN0 = 'W';
011317191121               ONTTOTAL = -1;
011318191121             ENDIF;
011319191121
011320191121             EXEC SQL
011321191121               SELECT COUNT(*) INTO :QUETOTAL
011322191121               FROM BCI433LIB/QUEBEC
011323191122               WHERE PURCHASE <= :LOWLIMIT;
011324191121
011325191121             IF SQLCODE <> 0 OR SQLWN0 = 'W';
011326191121               QUETOTAL = -1;
011327191121             ENDIF;
011328191121
011329191121             EXEC SQL
011330191121               SELECT COUNT(*) INTO :MANTOTAL
011331191121               FROM BCI433LIB/MANITOBA
011332191122               WHERE PURCHASE <= :LOWLIMIT;
011333191121
011334191121             IF SQLCODE <> 0 OR SQLWN0 = 'W';
011335191121               MANTOTAL = -1;
011336191121             ENDIF;
011337191121
014600191121           ENDSR;
014601190315
014602191121
014603190315
014604190315
014605190315
014606190315
014607190315
014608190315
014609190315
014610190315
014611190315
014612190315
014613190315
014614190315
014615190315
