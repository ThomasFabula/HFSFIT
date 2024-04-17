program HFSFIT;
(* ***************************************************************** *)
(*                                                                   *)
(*     PROGRAMM : HFSFIT                                             *)
(*                                                                   *)
(*     Prog. 1 : S. Büttgenbach, F. Träber  (HFSFIT.FOR)             *)
(*     Prog. 2 : H. Roeder                  (HFSFIT.PAS)             *)
(*     Prog. 3 : T. Fabula                  (PC: HFS.PAS)            *)
(*                                                                   *)
(*                                                                   *)
(*     !!! ACHTUNG : !!!                                             *)
(*     =================                                             *)
(*     Für die PC-Version  i s t  unbedingt die Eingabedatei         *)
(*     HFS.IN  nötig.                                                *)
(*     Der Output erfolgt in die Ausgabedatei  HFS.OUT !             *)
(*     Startwerte für das Programm  HFSPEK  werden in die            *)
(*     Datei  HFSSPEK.PAR  geschrieben.                              *)
(*                                                                   *)
(*     DIESES PROGRAMM BERECHNET AUS EINGEGEBENEN MESSWERTEN         *)
(*     HFS-PARAMETER . DIE PARAMETER TRAGEN DABEI FOLGENDE           *)
(*     INTERNE NUMMERIERUNG  :                                       *)
(*                                                                   *)
(*         PARAMETER-NR.          PARAMETER-BEZ.                     *)
(*                                                                   *)
(*               1             S1       SCHWERPUNKT DES 1.TEN ISOT.  *)
(*               2             A(OBEN)  A-FAKTOR          "          *)
(*               3             A(UNTEN) A-FAKTOR          "          *)
(*               4             B(OBEN)  B-FAKTOR          "          *)
(*               5             B(UNTEN) B-FAKTOR          "          *)
(*                                                                   *)
(*               6             S1       SCHWERPUNKT DES 2.TEN ISOT.  *)
(*               7             A(OBEN)  A-FAKTOR          "          *)
(*               8             A(UNTEN) A-FAKTOR          "          *)
(*               9             B(OBEN)  B-FAKTOR          "          *)
(*              10             B(UNTEN) B-FAKTOR          "          *)
(*                                                                   *)
(*                                                                   *)
(*     DABEI KOENNEN EINZELNE PARAMETER RELATIV ZU EINEM ANDEREN     *)
(*     FESTGEHALTEN WERDEN, ODER AUCH ABSOLUT AUF EINEN BESTIMMTEN   *)
(*     WERT GEHALTEN WERDEN.                                         *)
(*                                                                   *)
(*                                                                   *)
(*       WENN ALLE HFS-KONSTANTEN FESTGEHALTEN WERDEN, WIRD KEIN FIT *)
(*    DURCHGEFUEHRT. ES WERDEN STATTDESSEN DIE FREQUENZEN UND REL.   *)
(*    INTENSITAETEN DER EINZELNEN KOMPONENTEN BERECHNET UND BUCH-    *)
(*    STABEN BEZEICHNUNGEN FUER DIE KOMPONENTEN ERMITTELT.           *)
(*                                                                   *)
(*    BESCHRAENKUNGEN DES PROGRAMMES:                                *)
(*                                                                   *)
(*           MAXIMALE ANZAHL VON MESSWERTEN = 100                    *)
(*                                                                   *)
(*           ES IST AUCH NICHT ERLAUBT EINEN PARAMETER AUF EINEN     *)
(*           PARAMTER FESTZUHALTEN, DER SELBST DURCH EIN VERHAELTNIS *)
(*           BESTIMMT IST. NATUERLICH IST ES AUCH NICHT ERLAUBT      *)
(*           SCHLEIFEN ZU BILDEN .                                   *)
(*                                                                   *)
(*                                                                   *)
(*    EINGABE - DATEN  (HFS.IN) :                                    *)
(*                                                                   *)
(*    ZEILE     SPALTE    FORMAT     NAME      BEZEICHNUNG           *)
(*                                                                   *)
(*                                                                   *)
(*     1                    I       NISOTOP    ANZAHL ISOTOPE        *)
(*     1                    I       FIXED_PAR  ANZAHL FESTGEHALTENER *)
(*                                             PARAMETER             *)
(*     1                    R         XJO      J DES OBEREN ZUSTANDES*)
(*     1                    R         XJU      J DES UNTEREN ZUST.   *)
(*     1                    R         XI1      I KERNSPIN 1.TE ISO   *)
(*     1                    R         XI2      I KERNSPIN 2.TE ISO.  *)
(*                                             DIESER WERT MUSS AUCH *)
(*                                             EINGEGEBEN WERDEN     *)
(*                                             WENN NISOTOP = 1 IST  *)
(*                                                                   *)
(*                                                                   *)
(*  WENN PARAMETER FESTGEHALTEN WERDEN FOLGT FUER JEDEN DER          *)
(*  'FIXED_PAR' PARAMETER EIN ZEILE VOM TYP 2.                       *)
(*                                                                   *)
(*     2                    I       NFP[I]      NR. DES FESTGEH.     *)
(*                                              PARAMETERS           *)
(*     2                    R       VALFP[I]  WERT ODER VERHAELTNIS  *)
(*                                              DES FESTEN PAR.      *)
(*     2                    I       REFP[I]     BEZUGS-PARAMETER     *)
(*                                              IST REFP[I] = 0 SO   *)
(*                                              WIRD DER PARAMETER   *)
(*                                             AUF DEN WERT VALFP[I] *)
(*                                              FESTGEHALTEN         *)
(*                                                                   *)
(*   PARAMETER DUERFEN AUCH UEBER EIN VERHAELTNIS AUF EINEN ABSOLUT  *)
(*   FESTGEHALTENEN PARAMETER BEZOGEN WERDEN. NICHT ERLAUBT IST      *)
(*   JEDOCH DER BEZUG AUF EINEN 'RELATIV' FESTGEHALTENEN PARAMETER.  *)
(*                                                                   *)
(*   ES FOLGEN DIE ZEILEN MIT DEN MESSWERTEN                         *)
(*                                                                   *)
(*     3        1           C1    ISOTOP[I] 2 =  DATUM GEHOERT ZUM   *)
(*                                               2.TEN ISOTOP.       *)
(*                                               ZUM 1.TEN ISOTOP .  *)
(*                                            BELIEBIG = DATUM GE-   *)
(*                                               HOERT ZUM 1.TEN     *)
(*                                               ISOTOP              *)
(*                                                                   *)
(*     3       2-5          C4    XNAME[I]   BEZEICHNUNG DES I.TEN   *)
(*                                             EINGABE WERTES        *)
(*     3        6           C1    LIMIT        WIRD HIER EIN CHAR.   *)
(*                                             UNGLEICH BLANK EINGE- *)
(*                                             TRAGEN SO WIRD DIES   *)
(*                                             ALS ENDE DES DATEN-   *)
(*                                             SATZES VERSTANDEN     *)
(*     3                    R       FO[I]     F-QUANTENZAHL          *)
(*     3                    R       FU[I]     F-QUANTENZAHL          *)
(*     3                    R       Y[I]      MESSWERT               *)
(*     3                    R       DY[I]     FEHLER D. MESSWERTES   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(*                                                                   *)
(* ***************************************************************** *)

const PMAX   = 14 ;         (* MAXIMAL ZAHL VON PARAMETERN + 4 *)
      WMAX   = 100 ;        (* MAXIMAL ZAHL VON WERTEPAAREN    *)

      datin  = 'HFS.IN' ;       (* Datei der Eingabeparameter  *)
      datout = 'HFS.OUT';       (*   "    "  berechneten Werte *)
      param  = 'HFS.PAR';       (* Parameterdatei fr HFSFIT   *)
      spek   = 'HFSSPEK.PAR';   (* Parameterdatei fr HFSSPEK  *)

type REAL8 = REAL ;
     TYPE_QMATRIX = ARRAY [1..PMAX,1..PMAX] OF REAL8 ;
     TYPE1_MATRIX = TYPE_QMATRIX  ;
     TYPE2_MATRIX = ARRAY [1..WMAX,1..PMAX] OF REAL8 ;
     TYPE3_MATRIX = TYPE2_MATRIX  ;
     TYPEX_VEKTOR = ARRAY [1..WMAX] OF REAL8 ;
     TYPEXN_VEKTOR = ARRAY [1..WMAX] OF PACKED ARRAY[1..4] OF CHAR ;
     TYPEISO_VEKTOR = ARRAY [1..WMAX] OF CHAR ;
     TYPEP_VEKTOR = ARRAY [1..PMAX] OF REAL8 ;
     TYPEPINT_VEKTOR = ARRAY [1..PMAX] OF INTEGER ;
     TYPE_KOPF    = PACKED ARRAY [1..8] OF CHAR ;
     KOMMENTAR    = PACKED ARRAY[1..70] OF CHAR ;
     TYPE_PNAME   = ARRAY[-1..PMAX] OF PACKED ARRAY[1..10] OF CHAR ;
     str12        = string[12];
var
    ind,out,fout : text;   (*  File Variablen für ASCII-Dateien  *)

    CA                                        : TYPE_QMATRIX ;
    DER,DER2                                  : TYPE2_MATRIX ;
    FO,FU,Y,DY,YY                             : TYPEX_VEKTOR ;
    XNAME                                     : TYPEXN_VEKTOR ;
    PNAME                                     : TYPE_PNAME ;
    NFP,REFP,SORT                             : TYPEPINT_VEKTOR ;
    ISOTOP                                    : TYPEISO_VEKTOR ;
    P,PP,VALFP                                : TYPEP_VEKTOR ;
    COMMENT                                   : KOMMENTAR ;
    I,J,K,NIT,R,RR,N,OPT,IER                  : INTEGER ;
    D,CHI,CHIO,XJO,XJU,XI1,XI2                : REAL8 ;
    KOPF                                      : TYPE_KOPF ;
    CC                                        : CHAR ;
    GAUSS,FLAG                                : BOOLEAN ;
    FIXED_PAR,NISOTOP                         : INTEGER ;
    element,vonzustand,nachzustand            : str12;
    defaultelement,defaultvon,defaultnach     : str12;

(* ***************************************************************** *)

procedure warten;
begin
  writeln;
  write('Bitte <<RETURN>> eingeben : ');
  read;
  clrscr;
end;

(* ***************************************************************** *)

function FAKULT ( N : INTEGER ) : REAL8 ;        (* FAKULT     *)
begin
  if N IN [0..33] then begin
    if N = 0 then FAKULT := 1
             else FAKULT := N * FAKULT (N-1) ;
  end else begin
    FAKULT := 0 ;
    writeln(#7,'Argument von Fakultt > als 33 , kleiner NULL oder ',
               'nicht ganzzahlig') ;
  end ;
end ;                                            (* end FAKULT *)

(* ***************************************************************** *)

procedure SECHSJ(     A1,A2,A3,B1,B2,B3  : REAL8 ; (* SECHSJ *)
                 var  RES                : REAL8 ;
                 var  IER                : INTEGER ) ;

var J1,J2,J3,L1,L2,L3                               : INTEGER ;
    T1,T2,T3,T4,T5,T6                               : REAL8 ;
    M11,M12,M13,M21,M22,M23,M31,M32,M33,M41,M42,M43 : INTEGER ;
    M51,M52,M53,M54,M61,M62,M63                     : INTEGER ;
    D1,D2,D3,D4,D,W1,W2,W3                          : REAL8 ;
    KA,KE,L,I,VOR                                   : INTEGER ;

   function DD ( M1,M2,M3,M4 : INTEGER ) : REAL8 ;
   begin
      DD := SQRT( FAKULT(M1) * FAKULT(M2) * FAKULT(M3) / FAKULT(M4+1) )
   end ;

begin
  RES := 0.0 ;
  J1 :=  ROUND(2.0 * A1 ) ;       (* QUANTENZAHLEN -> INTEGER *)
  J2 :=  ROUND(2.0 * A2 ) ;
  J3 :=  ROUND(2.0 * A3 ) ;
  L1 :=  ROUND(2.0 * B1 ) ;
  L2 :=  ROUND(2.0 * B2 ) ;
  L3 :=  ROUND(2.0 * B3 ) ;
  T1 := J1 ;                      (* TEST AUF HALBZAHLIGKEIT *)
  T2 := J2 ;
  T3 := J3 ;
  T4 := L1 ;
  T5 := L2 ;
  T6 := L3 ;
  T1 := ABS(2.0 * A1 - T1 ) ;
  T2 := ABS(2.0 * A2 - T2 ) ;
  T3 := ABS(2.0 * A3 - T3 ) ;
  T4 := ABS(2.0 * B1 - T4 ) ;
  T5 := ABS(2.0 * B2 - T5 ) ;
  T6 := ABS(2.0 * B3 - T6 ) ;
  if (T1+T2+T3+T4+T5+T6) > 0.001 then begin
     IER := 2  ;
     writeln(#7,'Quantenzahl NICHT halb- oder ganzzahlig.') ;
  end else begin                       (* BERECHNUNG DES 6-J SYMBOLS *)
     M11 := (J1+J2-J3) DIV 2 ;
     M12 := (J1+J3-J2) DIV 2 ;
     M13 := (J2+J3-J1) DIV 2 ;
     M21 := (J1+L2-L3) DIV 2 ;
     M22 := (J1+L3-L2) DIV 2 ;
     M23 := (L2+L3-J1) DIV 2 ;
     M31 := (L1+J2-L3) DIV 2 ;
     M32 := (L1+L3-J2) DIV 2 ;
     M33 := (L3+J2-L1) DIV 2 ;
     M41 := (L1+L2-J3) DIV 2 ;
     M42 := (L1+J3-L2) DIV 2 ;
     M43 := (L2+J3-L1) DIV 2 ;
     M51 := (J1+J2+J3) DIV 2 ;
     M52 := (J1+L2+L3) DIV 2 ;
     M53 := (L1+J2+L3) DIV 2 ;
     M54 := (L1+L2+J3) DIV 2 ;
     M61 := (J1+J2+L1+L2) DIV 2 ;
     M62 := (J2+J3+L2+L3) DIV 2 ;
     M63 := (J3+J1+L3+L1) DIV 2 ;
     D1 := DD (M11,M12,M13,M51) ;
     D2 := DD (M21,M22,M23,M52) ;
     D3 := DD (M31,M32,M33,M53) ;
     D4 := DD (M41,M42,M43,M54) ;
     D := D1*D2*D3*D4 ;
     KA := M51 ;
     if M52 > KA then KA := M52 ;
     if M53 > KA then KA := M53 ;
     if M54 > KA then KA := M54 ;
     if KA = 0 then RES := 1.0
     else begin
       KE := M61 ;
       if M62 < KE then KE := M62 ;
       if M63 < KE then KE := M63 ;
       if KA > KE then IER := 3
       else begin
         for L := KA TO KE do begin
            if ODD (L) then W1 := -1
                       else W1 := 1 ;
            W1 := W1 * FAKULT(L+1) ;
            W2 := FAKULT(L-M51)*FAKULT(L-M52)*FAKULT(L-M53)*
                  FAKULT(L-M54) ;
            W3 := FAKULT(M61-L)*FAKULT(M62-L)*FAKULT(M63-L) ;
            RES := RES + W1/(W2*W3) ;
         end ;
         RES := RES * D ;
         IER := 0 ;
       end ;
     end ;
   end ;
end ;                                            (* end SECHSJ *)

(* ***************************************************************** *)

procedure MATOUT ( var A     : TYPE_QMATRIX ;    (* MATOUT *)
                   var M,N   : INTEGER      ;
                       W     : INTEGER     );
(*    A  MATRIX VOM RANG (M x N) DIE AUF FILE OUT AUSGEGEBEN *)
(*       WERDEN SOLL.                                        *)
(*  M,N  RANG DER  MATRIX A                                  *)
(*   W   ANZAHL DER WERTE PRO ZEILE                          *)

var I,J,Z,K : INTEGER ;

begin
  K := TRUNC(70/(W+2)) ;
  Z := 1;
  for I := 1 TO M do begin
    write(I:2,'/ 1  ');
    for J := 1 TO N do begin
      write(A[I,J]:K,'  ') ;
      if ((J MOD W = 0 ) AND ( J <> N )) then begin
        writeln ;
        write('   /',J+1:2,'  ') ;
      end ;
    end ;
    writeln ;
  end ;
  writeln ;
end ;                                             (* ENDE MATOUT *)

(* ***************************************************************** *)
(*                 Dateiausgabe der Werte   -->  out                 *)
(* ***************************************************************** *)

procedure MATOUTdat ( var A     : TYPE_QMATRIX ;
                   var M,N   : INTEGER      ;
                       W     : INTEGER     );
(*    A  MATRIX VOM RANG (M x N) DIE AUF FILE OUT AUSGEGEBEN *)
(*       WERDEN SOLL.                                        *)
(*  M,N  RANG DER  MATRIX A                                  *)
(*   W   ANZAHL DER WERTE PRO ZEILE                          *)

var I,J,Z,K : INTEGER ;

begin
  K := TRUNC(70/(W+2)) ;
  Z := 1;
  for I := 1 TO M do begin
    write(out,I:2,'/ 1  ');
    for J := 1 TO N do begin
      write(out,A[I,J]:K,'  ') ;
      if ((J MOD W = 0 ) AND ( J <> N )) then begin
        writeln(out) ;
        write(out,'   /',J+1:2,'  ') ;
      end ;
    end ;
    writeln(out) ;
  end ;
  writeln(out) ;
end ;

(* ***************************************************************** *)

procedure MATINV ( var B,A : TYPE_QMATRIX ;        (* MATINV    *)
                       N   : INTEGER      ;
                   var D   : REAL8    )   ;

                     (*  B   : INPUT MATRIX      *)
                     (*  A   : INVERSE MATRIX    *)
                     (*  D   : DETERMINATE       *)
                     (*  N   : ORDNUNG VON B,A   *)

label  9999 ;
var   L,ML      : ARRAY [1..PMAX] OF INTEGER ;
      BIGA,HOLD : REAL8 ;
      KK,I,J     : INTEGER ;

  procedure CHANGE ( var A,B : REAL8 ) ;
  var C : REAL8 ;
  begin
    C := -A ;
    A := B ;
    B := C ;
  end ;

begin
  for I := 1 TO N do (* KOPIERE INPUTMATRIX NACH MATINV (A) *)
    for J := 1 TO N do A[I,J] := B[I,J] ;
  D := 1.0 ;
  for KK := 1 TO N do begin
    L[KK]:= KK ;
    ML[KK] := KK ;
    BIGA := A[KK,KK] ;
    for J := KK TO N do
      for I := KK TO N do if ABS(BIGA) < ABS(A[I,J]) then begin
        BIGA := A[I,J] ;
        L[KK] := I ;
        ML[KK] := J ;
    end ;
    J := L[KK] ;              (*  INTERCHANGE ROWS *)
    if J > KK then  for I := 1 TO N do CHANGE (A[KK,I],A[J,I]) ;
    I := ML[KK] ;              (*  INTERCHANGE COLUMNS *)
    if I > KK then  for J := 1 TO N do CHANGE (A[J,KK],A[J,I]) ;
    if BIGA = 0 then begin   (* DIVIDE COLUMN BY -BIGA *)
      D := 0.0 ;
      goto 9999 ;
    end ;
    for I := 1 TO N do if I <> KK then A[I,KK] := -A[I,KK]/BIGA ;
    for I := 1 TO N do begin (* REDUCE MATRIX *)
      HOLD := A [I,KK] ;
      for J := 1 TO N do
        if ((I<>KK)AND(J<>KK)) then A[I,J] := HOLD*A[KK,J]+A[I,J] ;
    end ;
    for J := 1 TO N do        (* DIVIDE ROW BY BIGA *)
      if J <> KK then A[KK,J] := A[KK,J] / BIGA ;
    D := D * BIGA ;
    A[KK,KK] := 1 / BIGA ;
  end ;
           (* FINAL ROW AND COLUMN INTERCHANGE *)
  if N > 1 then  for KK := N-1 doWNTO 1 do begin
    I := L[KK] ;
    if I > KK then for J := 1 TO N do CHANGE (A[J,I],A[J,KK]) ;
    J := ML[KK] ;
    if J > KK then for I := 1 TO N do CHANGE (A[J,I],A[KK,I]) ;
  end ;
9999 : end ;                                     (* end MATINV *)

(* ***************************************************************** *)

procedure MATMULT (var MATPROD : TYPE1_MATRIX ;  (* MATMULT *)
                   var MAT1    : TYPE2_MATRIX ;
                   var MAT2    : TYPE3_MATRIX ;
                   var SP1,Z1,Z2 : INTEGER ) ;

(* ES WIRD DAS PRODUKT MATPROD = MAT1 * MAT2 AUSGEFUEHRT     *)
(* MAT1 HAT DEN RANG(SP1 X Z1 ) UND MAT2 DEN RANG (Z1 X Z2 ) *)

var I,J,K : INTEGER ;

begin
  for I := 1 TO SP1 do begin
    for J := 1 TO Z2 do begin
      MATPROD[I,J] := 0.0 ;
      for K := 1 TO Z1 do
        MATPROD[I,J] := MATPROD[I,J] + MAT1[I,K] * MAT2[J,K] ;
    end ;
  end ;
end ;                                              (* end MATMULT *)

(* ***************************************************************** *)

procedure MAVEKMULT( var A   : TYPE_QMATRIX ;      (* MAVEKMULT *)
                     var V,P : TYPEP_VEKTOR ;
                         N,M : INTEGER ) ;

var I,J : INTEGER ;

begin
  for I := 1 TO N do begin
    P[I] := 0.0 ;
    for J := 1 TO M do P[I] := P[I] + A[I,J] * V[J] ;
  end ;
end ;                                             (* end MAVEKMULT *)

(* ***************************************************************** *)

function YCALC ( var DER : TYPE2_MATRIX ;         (* YCALC *)
                 var P   : TYPEP_VEKTOR ;
                     I,R : INTEGER ) : REAL8 ;

var J : INTEGER ;
   AUX : REAL8 ;

begin
  AUX := 0.0 ;
  for J := 1 TO R do AUX := AUX + DER[I,J] * P[J] ;
  YCALC := AUX ;
end ;                                            (* end YCALC *)

(* ***************************************************************** *)

procedure LEASTS ( var DER     : TYPE2_MATRIX ;  (* LEASTS *)
                   var DY,YY   : TYPEX_VEKTOR ;
                       N,R     : INTEGER      ;
                   var CB      : TYPE_QMATRIX ;
                   var PP,P    : TYPEP_VEKTOR ;
                   var CHI     : REAL8        ;
                   var IER     : INTEGER    ) ;

(*    DER       KOEFFIZIENTENMATRIX (N X R )                  *)
(*    DY,Y      FEHLER- UND WERTEVEKTOR (N)                   *)
(*    N         ZAHL DER MESSUNGEN                            *)
(*    R         ZAHL DER PARAMETER                            *)
(*    CB        KOVARIANZMATRIX   ( R X R )                   *)
(*    PP,P      PARAMETERVEKTOR,BZW. KOREKTUREN               *)
(*    CHI       CHI-QUADRAT                                   *)
(*    IER       FEHLERCODE   = 0  ALLES O.K.                  *)
(*                           = 4  PARAMETERZAHL IST NULL      *)
(*                           = 3  DETERMINANTE NULL           *)
(*                           = 1  WENIGER ALS 1 FREIHEITSGRAD *)

label 9998 ;
var I,J,K  : INTEGER ;
    YN     : TYPEP_VEKTOR ;
    D,H0   : REAL8 ;
    Q,II   : TYPE_QMATRIX ;

begin                (* TEST AUF FEHLER *)
  IER := 0 ;
  if R < 1 then begin
    IER := 4 ;
    writeln(#7,'Es sollen WENIGER als 1 Parameter gefittet werden :') ;
    goto 9998 ;
  end ;
  if N < R then begin
    IER := 1 ;
    writeln(#7,'Die Zahl der Freiheitsgrade ist KLEINER NULL :') ;
    goto 9998 ;
  end ;
  for I := 1 TO R do begin       (* BERECHNUNG DER VARIANZ-MATRIX *)
    for J := 1 TO R do begin
      Q[I,J] := 0.0 ;
      for K := 1 TO N do begin
        if DY[K] = 0 then DY[K] := 1.0 ;
        Q[I,J] := Q[I,J] + DER[K,I] / DY[K] / DY[K] * DER[K,J] ;
      end ;
    end ;
    YN[I] := 0.0 ;               (* UND DES VARIANZVEKTORS *)
    for K := 1 TO N do
      YN[I] := YN[I] + DER[K,I] / DY[K] / DY[K] * YY[K] ;
  end ;
  MATINV(Q,CB,R,D) ;             (* BILDUNG DER KOVARIANZMATRIX *)
  if D = 0 then begin
    IER := 3 ;
    writeln(#7,'DIE DETERMINATE IST NULL  :') ;
    goto 9998 ;
  end ;
  MAVEKMULT(CB,YN,P,R,R) ;    (* BERECHNUNG DER PARAMETERKORREKTUREN *)
  for J := 1 TO R do  PP[J] := PP[J] + P[J] ;
  CHI := 0.0 ;                  (* BERECHNUNG VON CHI-QUADRAT *)
  for I := 1 TO N do begin
    D := (YY[I]-YCALC(DER,PP,I,R) ) /DY[I] ;
    CHI := CHI + D * D ;
  end ;
9998 : end ;                                     (* end LEASTS *)

(* ***************************************************************** *)

function C ( var F,J,I : REAL8 ) :REAL8 ;        (* HFSKOEF *)

begin
   C := F * (F+1) -I * (I+1) - J * (J+1 ) ;
end ;

function COEFA( var F,J,I : REAL8 ) : REAL8 ;
begin
   COEFA := C(F,J,I ) / 2.0 ;
end ;

function COEFB ( var F,J,I: REAL8) : REAL8 ;
var AUX : REAL8 ;
begin
   AUX := 2 * I * (2*I-1) * J * (2*J-1) ;
   if ABS(AUX) < 0.001 then COEFB := 0.0
   else begin
      AUX := ( 0.75 * C(F,J,I)*( C(F,J,I)+1 ) - I*(I+1)*J*(J+1) )/AUX ;
      COEFB := AUX ;
   end ;
end ;                                            (* end HFSKOEF *)

(* ***************************************************************** *)

procedure DERIV ( var DER     : TYPE2_MATRIX  ;  (* DERIV *)
                  var XJO,XJU,XI1,XI2 : REAL8 ;
                  var FO,FU   : TYPEX_VEKTOR ;
                  var ISOTOP  : TYPEISO_VEKTOR ;
                      N,R     : INTEGER ) ;

var I,J,K             : INTEGER ;
    AUX,H0,H1,H2,H3,XI : REAL8 ;
begin
   for I := 1 TO N do begin ;
     AUX := 1.0 ;
     if ISOTOP[I] = '2' then begin
        J := 5  ;
        XI := XI2 ;
     end else begin
        J := 0 ;
        XI := XI1 ;
     end ;
      DER [I,J+1] := 1.0 ;
      DER [I,J+2] := COEFA (FO[I],XJO,XI ) ;
      DER [I,J+3] := -COEFA (FU[I],XJU,XI ) ;
      DER [I,J+4] := COEFB (FO[I],XJO,XI ) ;
      DER [I,J+5] := -COEFB (FU[I],XJU,XI ) ;
   end ;
end ;                                            (* end DERIV *)

(* ***************************************************************** *)

procedure WRITECHAR  ( C : CHAR ;                (* WRITECHAR *)
                       N : INTEGER ) ;

var I : INTEGER ;

begin
  write(' ') ;
  for I := 1 TO N do write(C:1) ;
  writeln ;
  writeln ;
end ;                                            (* end WRITECHAR *)

(* ***************************************************************** *)
(*                 Dateiausgabe der Werte   -->  out                 *)
(* ***************************************************************** *)

procedure WRITEDATCHAR  ( C : CHAR ;
                       N : INTEGER ) ;

var I : INTEGER ;

begin
  write(out,' ') ;
  for I := 1 TO N do write(out,C:1) ;
  writeln(out) ;
  writeln(out) ;
end ;

(* ***************************************************************** *)

procedure WRITEOUT ( var P             : TYPEP_VEKTOR ; (* WRITEOUT *)
                     var CA            : TYPE_QMATRIX  ;
                     var XJO,XJU,XI1,XI2 : REAL8 ;
                     var FO,FU         : TYPEX_VEKTOR ;
                     var ISOTOP        : TYPEISO_VEKTOR ;
                     var Y,DY          : TYPEX_VEKTOR ;
                     var DER           : TYPE2_MATRIX ;
                         N,R,RR        : INTEGER ;
                     var CHI           : REAL8   ;
                     var KOPF          : TYPE_KOPF  ;
                     var COMMENT       : KOMMENTAR ;
                     var SORT,NFP,REFP : TYPEPINT_VEKTOR ;
                     var VALP          : TYPEP_VEKTOR  ;
                     var PNAME         : TYPE_PNAME  ;
                     var XNAME         : TYPEXN_VEKTOR ) ;

var I,J,K,FR     : INTEGER ;
    AUX          : REAL8 ;
    Q            : TYPE_QMATRIX ;
    PP,DP        : TYPEP_VEKTOR ;
    varIIERT     : PACKED ARRAY [1..8] OF CHAR ;

begin

  VARIIERT := 'variiert' ;
  writeln(' Es wurde ein ',KOPF,' durchgefhrt') ;
  WRITECHAR('-',70) ;
  writeln(' J(oben) =',XJO:6:1, '     J(unten) =',XJU:6:1,
              '      I1 =',XI1:6:1,'    I2 =' ,XI2:6:1);
  writeln;
  writeln(' Ergebnisse :') ;
  writeln(' ------------') ;
  for I := 1 TO RR do begin (* BERECHNUNG DER FESTGEHALTENEN PARAMETER *)
    K := SORT[I] ;
    if K > 0 then begin
      PP[I] := P[K] ;
      DP[I] := SQRT(ABS(CA[K,K])) ;
    end ;
  end ;
  for I := 1 TO RR-R do if REFP[I] = 0 then begin
     DP[NFP[I]] := 1.0 ;
     PP[NFP[I]] := VALP[I] ;
  end ;
  for I:= 1 TO RR-R do if REFP[I] > 0 then begin
     DP[NFP[I]] := VALP[I] * DP[REFP[I]] ;
     PP[NFP[I]] := VALP[I] * PP[REFP[I]] ;
  end ;
  writeln ;
  writeln(' CHI-Quadrat           = ',CHI:10:4) ;
  FR := N-R ;
  if FR <=1 then FR := 1 ;
  AUX := SQRT(CHI/FR)  ;
  writeln(' Anzahl Freiheitsgrade = ',FR:10) ;
  writeln(' RMS - Faktor          = ',AUX:10:4) ;
  writeln(' Es wurden ',R:2,' Parameter FREI variiert ') ;
  writeln ;
  writeln(' Ausgabe der GEFITTETEN Parameter und deren Fehler :') ;
  WRITECHAR('-',51) ;
  for I := 1 TO RR do begin
       write(PNAME[I]:10,' = ',PP[I]:15:5,'   (',DP[I]:15:7,
                 ')') ;
       if SORT[I] > 0 then writeln(VARIIERT:12)
                        else writeln(PNAME[SORT[I]]:14) ;
  end ;
  writeln;
  warten;
  writeln(' Ausgabe der RCKGERECHNETEN Werte und der Eingaben :') ;
  WRITECHAR('-',52) ;
  writeln('              Fo    Fu            Y             ',#127,'Y',
              '          YCALC        YCALC-Y') ;
  writeln ;
  for I := 1 TO N do begin
    AUX := YCALC(DER,PP,I,RR) ;
    writeln(' ',ISOTOP[I]:4,XNAME[I]:6,FO[I]:6:1,FU[I]:6:1,
                Y[I]:15:3,DY[I]:14:3,AUX:14:3,(AUX-Y[I]):14:3 ) ;
  end ;
  writeln;
  warten;
  writeln(' KOVARIANZMATRIX    :') ;
  WRITECHAR('-',20) ;
  MATOUT (CA,R,R,6) ;
  for I := 1 TO R do
    for J := 1 TO R do Q[I,J] := CA[I,J]/SQRT(ABS(CA[I,I]*
                                   CA[J,J])) ;
  writeln(' KORRELATIONSMATRIX :') ;
  WRITECHAR('-',20) ;
  MATOUT(Q,R,R,6) ;
end ;

(* ***************************************************************** *)
(* *****               Werte auf Platte schreiben             ****** *)
(* ***************************************************************** *)

procedure WRITEOUTDAT ( var P             : TYPEP_VEKTOR ;
                     var CA            : TYPE_QMATRIX  ;
                     var XJO,XJU,XI1,XI2 : REAL8 ;
                     var FO,FU         : TYPEX_VEKTOR ;
                     var ISOTOP        : TYPEISO_VEKTOR ;
                     var Y,DY          : TYPEX_VEKTOR ;
                     var DER           : TYPE2_MATRIX ;
                         N,R,RR        : INTEGER ;
                     var CHI           : REAL8   ;
                     var KOPF          : TYPE_KOPF  ;
                     var COMMENT       : KOMMENTAR ;
                     var SORT,NFP,REFP : TYPEPINT_VEKTOR ;
                     var VALP          : TYPEP_VEKTOR  ;
                     var PNAME         : TYPE_PNAME  ;
                     var XNAME         : TYPEXN_VEKTOR ) ;

var I,J,K,FR     : INTEGER ;
    AUX          : REAL8 ;
    Q            : TYPE_QMATRIX ;
    PP,DP        : TYPEP_VEKTOR ;
    VARIIERT     : PACKED ARRAY [1..8] OF CHAR ;

begin
  assign(out,datout);
  rewrite(out);
  writeln('Berechnete Werte werden in der Datei ',datout,' gespeichert.');
  VARIIERT := 'variiert' ;
  writeln(out,' HFSFIT  -  Berechnung der A-, B-Faktoren :');
  WRITEDATCHAR ( '=',42 ) ;
  writeln(out,' Element   : ',element ) ;
  writeln(out,' Uebergang : ',vonzustand,' -> ',nachzustand) ;
  writeln(out,' ',COMMENT);
  WRITEDATCHAR('-',70) ;
  writeln(out,' Es wurde ein ',KOPF,' erfolgreich durchgefuehrt.') ;
  WRITEDATCHAR('-',70) ;
  writeln(out,' J(oben) =',XJO:6:1, '     J(unten) =',XJU:6:1,
              '      I1 =',XI1:6:1,'    I2 =' ,XI2:6:1);
  writeln(out);
  writeln(out,' Ergebnisse :') ;
  WRITEDATCHAR('-',12);
  for I := 1 TO RR do begin (* BERECHNUNG DER FESTGEHALTENEN PARAMETER *)
    K := SORT[I] ;
    if K > 0 then begin
      PP[I] := P[K] ;
      DP[I] := SQRT(ABS(CA[K,K])) ;
    end ;
  end ;
  for I := 1 TO RR-R do if REFP[I] = 0 then begin
     DP[NFP[I]] := 1.0 ;
     PP[NFP[I]] := VALP[I] ;
  end ;
  for I:= 1 TO RR-R do if REFP[I] > 0 then begin
     DP[NFP[I]] := VALP[I] * DP[REFP[I]] ;
     PP[NFP[I]] := VALP[I] * PP[REFP[I]] ;
  end ;
  writeln(out,' CHI-Quadrat           = ',CHI:10:4) ;
  FR := N-R ;
  if FR <=1 then FR := 1 ;
  AUX := SQRT(CHI/FR)  ;
  writeln(out,' Anzahl Freiheitsgrade = ',FR:10) ;
  writeln(out,' RMS - Faktor          = ',AUX:10:4) ;
  writeln(out,' Es wurden ',R:2,' Parameter FREI variiert ') ;
  writeln(out) ;
  writeln(out,' Ausgabe der GEFITTETEN Parameter und deren Fehler :') ;
  WRITEDATCHAR('-',51) ;
  for I := 1 TO RR do begin
       write(out,PNAME[I]:10,' = ',PP[I]:15:5,'   (',DP[I]:15:7,
                 ')') ;
       if SORT[I] > 0 then writeln(out,VARIIERT:12)
                        else writeln(out,PNAME[SORT[I]]:14) ;
  end ;
  writeln(out);
  writeln(out,' Ausgabe der RUECKGERECHNETEN Werte und der Eingaben :') ;
  WRITEDATCHAR('-',53) ;
  writeln(out,'              Fo    Fu            Y             ',#127,'Y',
              '          YCALC        YCALC-Y') ;
  writeln(out) ;
  for I := 1 TO N do begin
    AUX := YCALC(DER,PP,I,RR) ;
    writeln(out,' ',ISOTOP[I]:4,XNAME[I]:6,FO[I]:6:1,FU[I]:6:1,
                Y[I]:15:3,DY[I]:14:3,AUX:14:3,(AUX-Y[I]):14:3 ) ;
  end ;
  writeln(out);
  writeln(out,' KOVARIANZMATRIX    :') ;
  WRITEDATCHAR('-',20) ;
  MATOUTdat (CA,R,R,6) ;
  for I := 1 TO R do
    for J := 1 TO R do Q[I,J] := CA[I,J]/SQRT(ABS(CA[I,I]*
                                   CA[J,J])) ;
  writeln(out,' KORRELATIONSMATRIX :') ;
  WRITEDATCHAR('-',20) ;
  MATOUTdat(Q,R,R,6) ;
  close(out);

  assign(out,spek);     (* Parameterdatei fr HFSSPEK *)
  rewrite(out);
  writeln('Parameterdatei ',spek,' wird gespeichert.');
  writeln(out,'Spektrumparameter: (Element/unt. Zust./ober. Zust./');
  writeln(out,'------------------   Ao/Au/Bo/Bu/Jo/Ju/I/Kommentar)');
  writeln(out,element);            (* Elementbezeichnung *)
  writeln(out,vonzustand);         (* unterer Zustand    *)
  writeln(out,nachzustand);        (* oberer  Zustand    *)
  for i:=2 to 5 do writeln(out,PP[I]:15:5);  (* A-, B-Faktoren *)
  writeln(out,xjo:4:1);      (* Jo oben    *)
  writeln(out,xju:4:1);      (* Ju unten   *)
  writeln(out,xi1:4:1);      (* I1         *)
  writeln(out,COMMENT);
  close(out);
end ;

(* ***************************************************************** *)

procedure WRITEBEZ ( var XJO,XJU,XI1,XI2 : REAL8 ;  (* WRITEBEZ *)
                     var NFP             : TYPEPINT_VEKTOR ;
                     var VALFP           : TYPEP_VEKTOR ;
                     var COMMENT         : KOMMENTAR ;
                         ISOTOPE,PMAX    : INTEGER ) ;

var FU,FO,FMAXO,FMINO,FMAXU,FMINU : REAL8 ;
    I,K,H,IER                     : INTEGER ;
    AUX,RES6MAX,INT,XI            : REAL8 ;
    CHARACTER                     : CHAR ;
    P                             : TYPEP_VEKTOR ;
    CCHR                          : PACKED ARRAY [1..26] OF CHAR ;

begin
   CHARACTER := ' ';
   CCHR := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ;
   warten;
   writeln(' ',COMMENT ) ;
   WRITECHAR ( '=',70 ) ;
   writeln(' Ausgabe der BERECHNETEN Frequenzen und deren Intensitten :') ;
   WRITECHAR('-',59 ) ;
   writeln('Komp:   Fo:     Fu:           Energie:   Intensitt:');
   writeln;
   for I := 1 TO PMAX do P[NFP[I]] := VALFP[I] ;
   for I := 1 TO ISOTOPE do begin
      if I = 2 then begin
         H := 5 ;
         XI := XI2 ;
      end  else begin
         H := 0 ;
         XI := XI1 ;
      end ;
      K := 1 ;
      FMAXU := XJU + XI ;
      FMINU := ABS(XJU-XI) ;
      FU := FMAXU ;
      repeat
         FMAXO := XJO + XI ;
         FMINO := ABS(XJO - XI) ;
         FO := FMAXO ;
         repeat
            if (ABS(FO-FU) <= 1) AND ((FO*FU) <> 0.0) then begin
               AUX := P[1+H] + COEFA(FO,XJO,XI) * P[2+H]
                             - COEFA(FU,XJU,XI) * P[3+H]
                             + COEFB(FO,XJO,XI) * P[4+H]
                             - COEFB(FU,XJU,XI) * P[5+H] ;
               SECHSJ(XJO,FO,XI,FU,XJU,1.0,INT,IER ) ;
               INT := (2*FU+1) * (2*FO+1) *INT * INT ;
               if (FO = FMAXO) AND (FU = FMAXU ) then RES6MAX := INT ;
               INT := INT / RES6MAX * 100 ;
               writeln (' ',CCHR[K],CHARACTER,FO:8:1,FU:8:1,AUX:20:5,
                            INT:10:2,'%') ;
               K := K+1
            end ;
            FO := FO - 1 ;
         until FO < FMINO ;
         FU := FU - 1 ;
      until FU < FMINU ;
      CHARACTER :='"' ;
      writeln ;
   end ;
end ;                                            (* end WRITEBEZ *)

(* ***************************************************************** *)
(* *****               Werte auf Platte schreiben             ****** *)
(* ***************************************************************** *)

procedure WRITEBEZDAT ( var XJO,XJU,XI1,XI2 : REAL8 ;
                     var NFP             : TYPEPINT_VEKTOR ;
                     var VALFP           : TYPEP_VEKTOR ;
                     var COMMENT         : KOMMENTAR ;
                         ISOTOPE,PMAX    : INTEGER ) ;

var FU,FO,FMAXO,FMINO,FMAXU,FMINU : REAL8 ;
    I,K,H,IER                     : INTEGER ;
    AUX,RES6MAX,INT,XI            : REAL8 ;
    CHARACTER                     : CHAR ;
    P                             : TYPEP_VEKTOR ;
    CCHR                          : PACKED ARRAY [1..26] OF CHAR ;

begin
   CHARACTER := ' ';
   CCHR := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' ;
   assign(out,datout);
   rewrite(out);
   writeln('Berechnete Werte werden in der Datei ',datout,' gespeichert.');
   writeln(out,' Berechnung des Hfs-Spektrums :');
   WRITEDATCHAR ( '=',30 ) ;
   writeln(out,'Element   : ',element ) ;
   writeln(out,'Uebergang : ',vonzustand,' -> ',nachzustand) ;
   writeln(out,' ',COMMENT ) ;
   WRITEDATCHAR ( '-',70 ) ;
   writeln(out);
   writeln(out,' Ausgabe der BERECHNETEN Frequenzen und deren Intensitaeten :') ;
   WRITEDATCHAR('-',60 ) ;
   writeln(out,'Komp:   Fo:     Fu:           Energie:   Intensitaet:');
   writeln(out);
   for I := 1 TO PMAX do P[NFP[I]] := VALFP[I] ;
   for I := 1 TO ISOTOPE do begin
      if I = 2 then begin
         H := 5 ;
         XI := XI2 ;
      end  else begin
         H := 0 ;
         XI := XI1 ;
      end ;
      K := 1 ;
      FMAXU := XJU + XI ;
      FMINU := ABS(XJU-XI) ;
      FU := FMAXU ;
      repeat
         FMAXO := XJO + XI ;
         FMINO := ABS(XJO - XI) ;
         FO := FMAXO ;
         repeat
            if (ABS(FO-FU) <= 1) AND ((FO*FU) <> 0.0) then begin
               AUX := P[1+H] + COEFA(FO,XJO,XI) * P[2+H]
                             - COEFA(FU,XJU,XI) * P[3+H]
                             + COEFB(FO,XJO,XI) * P[4+H]
                             - COEFB(FU,XJU,XI) * P[5+H] ;
               SECHSJ(XJO,FO,XI,FU,XJU,1.0,INT,IER ) ;
               INT := (2*FU+1) * (2*FO+1) *INT * INT ;
               if (FO = FMAXO) AND (FU = FMAXU ) then RES6MAX := INT ;
               INT := INT / RES6MAX * 100 ;
               writeln (out,' ',CCHR[K],CHARACTER,FO:8:1,FU:8:1,AUX:20:5,
                            INT:10:2,'%') ;
               K := K+1
            end ;
            FO := FO - 1 ;
         until FO < FMINO ;
         FU := FU - 1 ;
      until FU < FMINU ;
      CHARACTER :='"' ;
      writeln(out) ;
   end ;
   close(out);
end ;                                            (* end WRITEBEZDAT *)

(* ***************************************************************** *)

procedure CHECK( var NFP,REFP        : TYPEPINT_VEKTOR ; (* CHECK *)
                 var VALFP           : TYPEP_VEKTOR ;
                 var PNAME           : TYPE_PNAME ;
                 var XJO,XJU,XI1,XI2 : REAL8 ;
                 var N,NISOTOP       : INTEGER ;
                 var FLAG            : BOOLEAN ) ;

var I,J,K : INTEGER ;

   procedure ADD (var N : INTEGER ;
                      NR: INTEGER ) ;
   begin
      N := N+1 ;
      NFP[N] :=  NR ;
      VALFP[N] := 0.0 ;
      REFP[N] := 0 ;
   end ;

begin
   if (XJO < 1.0) OR (XI1 < 1.0 ) then ADD(N,4) ;
   if NISOTOP = 2 then
       if (XJO < 1.0) OR (XI2 < 1.0 ) then ADD(N,9) ;
   if (XJU < 1.0) OR (XI1 < 1.0 ) then ADD(N,5) ;
   if NISOTOP = 2 then
       if (XJU < 1.0) OR (XI2 < 1.0 ) then ADD(N,10) ;
   I := 1 ;
   if N > 1 then repeat
      J := 1 ;
      repeat
         if I <> J then begin
          if (REFP[I] = NFP[J]) AND (REFP[J] <> 0) then begin
            writeln(#7,'Parameter Nr. ',PNAME[NFP[I]],' wird auf',
                        ' einen Parameter festgehalten der weder') ;
            writeln(' VARIIERT wird noch ABSOLUT festgehalten wird.');
            FLAG := FALSE ;
            end;
            if NFP[I] = NFP[J] then begin
               if (REFP[I] = REFP[J]) AND (REFP[I] = 0 ) AND
                  (VALFP[I] = VALFP[J]) then begin
                  if J < N then for K := J+1 TO N do begin
                     NFP[K-1] := NFP[K] ;
                     VALFP[K-1] := VALFP[K] ;
                     REFP[K-1] := REFP[K] ;
                     J := J-1 ;
                  end ;
                  N := N-1 ;
               end else begin
                  writeln(#7,'Der Parameter ',PNAME[NFP[I]],
                  ' wird MEHRMALS festgehelten.') ;
                  FLAG := FALSE ;
               end ;
            end ;

         end ;
         J := J + 1 ;
      until J > N ;
      if NFP[I] = REFP[I] then begin
         writeln (#7,'Der Parameter ' ,PNAME[NFP[I]],
                 ' wird auf sich SELBST bezogen.' ) ;
         FLAG := FALSE ;
      end ;
      I := I + 1 ;
   until I > N ;
end ;                                         (* end CHECK *)

(* ***************************************************************** *)

procedure parameter_einlesen;
{ Parameter aus der Datei HFS.PAR einlesen }
begin
  assign(ind,param);
  reset(ind);
  readln(ind,defaultelement);
  readln(ind,defaultvon);
  readln(ind,defaultnach);
  readln(ind,comment);
  close(ind);
end;

procedure parameter_speichern;
{ Parameter in die Datei HFS.PAR schreiben }
begin
  writeln('Parameter werden in der Datei ',param,' gespeichert.');
  assign(fout,param);
  rewrite(fout);
  writeln(fout,element);
  writeln(fout,vonzustand);
  writeln(fout,nachzustand);
  writeln(fout,comment);
  close(fout);
end;

(* ***************************************************************** *)

procedure READDATA ( var N,NISOTOP        : INTEGER ;    (* READDATA *)
                     var FIXED_PAR        : INTEGER ;
                     var FLAG             : BOOLEAN ;
                     var XJO,XJU,XI1,XI2  : REAL8 ;
                     var FO,FU            : TYPEX_VEKTOR ;
                     var ISOTOP           : TYPEISO_VEKTOR ;
                     var NFP,REFP         : TYPEPINT_VEKTOR ;
                     var VALFP            : TYPEP_VEKTOR ;
                     var Y,DY             : TYPEX_VEKTOR  ;
                     var XNAME            : TYPEXN_VEKTOR ) ;

var I,J,MAX : INTEGER ;
    C       : CHAR ;

begin
  FLAG := TRUE ;
  clrscr;
  writeln('Berechnung luft...');
  gotoxy(1,1);
  { Parameter aus HFS.IN einlesen }
  assign(ind,datin);
  reset(ind);
  readln(ind,NISOTOP,FIXED_PAR,XJO,XJU,XI1,XI2) ;
  MAX := 5 ;
  if NISOTOP >2 then NISOTOP := 2 ;
  if NISOTOP <1 then NISOTOP := 1 ;
  MAX := NISOTOP * MAX ;
  if (XJO < 1.0) OR(XI1<1.0) then MAX := MAX - 1 ;
  if (XJU < 1.0) OR(XI1<1.0) then MAX := MAX - 1 ;
  if (XJO < 1.0) OR(XI2<1.0) then MAX := MAX - 1 ;
  if (XJU < 1.0) OR(XI2<1.0) then MAX := MAX - 1 ;

  if FIXED_PAR <> 0  then begin
     writeln('Es wurden folgende ',FIXED_PAR:2,' Parameter ',
                 'festgehalten :') ;
     writeln('FIXED Parameter        Wert/Verhltnis   ',
                 '    Bezugs-Parameter') ;
     writeln ;
     for I := 1 TO FIXED_PAR do begin
       READLN(ind,NFP[I],VALFP[I],REFP[I] ) ;
       writeln(' ',PNAME[NFP[I]]:10,VALFP[I]:25:5,
                   PNAME[REFP[I]]:20 );
     end ;
  end ;

  N := 1 ;
  if not EOF(ind) then begin
     repeat                                 (* EINLESEN DER WERTE *)
       READLN (ind, ISOTOP[N] ,XNAME[N],C, FO[N] ,FU[N] , Y[N] ,
                DY[N]) ;
       if DY[N] <= 0.0 then DY[N] := 1 ;
       N := N + 1 ;
     until (EOF(ind) OR (N > WMAX) OR ( C <> ' ' ));
  end ;

  N := N-1 ;
  if N >= WMAX
    then writeln(#7,'Anzahl der Werte gráer als erlaubtes Maximum.');
  close(ind);
end ;

(* ***************************************************************** *)

procedure MODDER ( var D  ,DER2      : TYPE2_MATRIX ; (* MODDER *)
                   var NPAR,FIXP     : INTEGER      ;
                   var NFP,REFP,SORT : TYPEPINT_VEKTOR ;
                   var VALFP         : TYPEP_VEKTOR  ;
                   var Y,YY          : TYPEX_VEKTOR ) ;

var I,J,K,L,M : INTEGER ;
    FLAG      : BOOLEAN ;
    DER       : TYPE2_MATRIX ;
    SORTHELP  : TYPEPINT_VEKTOR ;

begin
   DER := D ;
   L := 1 ;
   YY := Y ;
   for I := 1 TO NPAR do begin
     FLAG := FALSE ;
     if FIXP >0 then for K := 1 TO FIXP do if NFP[K] = I then begin
        FLAG := TRUE ;
        M := K ;
     end ;
     if FLAG then begin
        if REFP[M] = 0 then for J := 1 TO N do begin
           SORT[I] := 0 ;
           SORTHELP[I] := M ;
        end else for J := 1 TO N do begin
           SORT[I] := -1 ;
           DER[J,REFP[M]] := DER[J,REFP[M]] + VALFP[M]*
                                 DER[J,NFP[M]] ;
        end ;
     end else begin
        SORT[I] := L ;
        L := L + 1 ;
     end ;
   end ;
   for I := 1 TO NPAR do begin
      if SORT[I] > 0 then for J := 1 TO N do DER2[J,SORT[I]]
       := DER[J,I] ;
      if SORT[I] = 0 then for J := 1 TO N do
         YY[J] := YY[J] - DER[J,NFP[SORTHELP[I]]] *
          VALFP[SORTHELP[I]] ;
   end ;
   NPAR := NPAR - FIXP ;
end ;                                            (* end MODDER *)

(* ***************************************************************** *)

procedure intro;
var  ok : char;
     ij : integer;
     strx,strx1,strx2 : str12;
begin
  clrscr;
  highvideo;
  write(#201); for ij:=1 to 78 do write(#205); write(#187);
  gotoxy(1,2);
  write(#186,'  HFSFIT   -   Berechnung der A-, B-Faktoren / ');
  write('Hfs-Spektrums eines Elementes');
  gotoxy(80,2); write(#186);
  gotoxy(1,3);
  write(#200); for ij:=1 to 78 do write(#205); write(#188);
  lowvideo;

  gotoxy(1,15);
  for ij:=1 to 80 do write(#205);
  gotoxy(1,17);
  for ij:=1 to 80 do write(#205);
  gotoxy(1,16);
  write(comment);

  repeat
    gotoxy(1,5);
    write('Bitte die Daten des Elementes eingeben :');
    gotoxy(1,7);
    write(' Welches Element (z.B.: Zirkon) : ',defaultelement);
    gotoxy(35,7);
    readln(strx);
    if ((strx<>defaultelement) and (length(strx)<>0))
     then element:=strx
     else element:=defaultelement;
    gotoxy(1,9);
    writeln('Welcher bergang soll betrachtet werden ?');
    gotoxy(1,10);
    write(' bergang von Zustand           : ',defaultvon);
    gotoxy(35,10);
    readln(strx1);
    gotoxy(1,11);
    write(' nach Zustand                   : ',defaultnach);
    gotoxy(35,11);
    readln(strx2);
    if ((strx1<>defaultvon) and (length(strx1)<>0))
     then vonzustand:=strx1
     else vonzustand:=defaultvon;
    if ((strx2<>defaultvon) and (length(strx2)<>0))
     then nachzustand:=strx2
     else nachzustand:=defaultnach;
    gotoxy(1,14);
    write(#7);
    write('Alle Parameter richtig (J/N) ? ');readln(ok);
    ok:=upcase(ok);
  until (ok='J');
end;

(************************************************************)
(*                                                          *)
(*            H A U P T P R O G R A M M                     *)
(*                                                          *)
(************************************************************)
begin
  KOPF := ' HFSFIT '  ;
  PNAME[-1]:= 'relativ   ' ;
  PNAME[0] := 'fixed     ' ;
  PNAME[1] := 'S1        ' ;
  PNAME[2] := 'A1 (oben) ' ;
  PNAME[3] := 'A1 (unten)' ;
  PNAME[4] := 'B1 (oben) ' ;
  PNAME[5] := 'B1 (unten)' ;
  PNAME[6] := 'S2        ' ;
  PNAME[7] := 'A2 (oben) ' ;
  PNAME[8] := 'A2 (unten)' ;
  PNAME[9] := 'B2 (oben) ' ;
  PNAME[10]:= 'B2 (unten)' ;
  parameter_einlesen;
  intro;
    READDATA (N,NISOTOP,FIXED_PAR,FLAG,XJO,XJU,XI1,XI2,FO,FU,
              ISOTOP,NFP,REFP,VALFP,Y,DY,XNAME ) ;
    CHECK(NFP,REFP,VALFP,PNAME,XJO,XJU,XI1,XI2,FIXED_PAR,NISOTOP,FLAG);
    if ((XI1<=0.0) OR (XJO < 0.0) OR (XJU < 0.0) OR (XI2 < 0.0))
      then
        writeln(#7,'Falsche Quantenzahl fr Kern- oder Hllenspin.')
       else if FLAG then begin
       if FIXED_PAR = NISOTOP * 5  then
         begin
          WRITEBEZ(XJO,XJU,XI1,XI2,NFP,VALFP,COMMENT,NISOTOP,FIXED_PAR);
          WRITEBEZDAT(XJO,XJU,XI1,XI2,NFP,VALFP,COMMENT,NISOTOP,FIXED_PAR);
         end else begin
          RR := NISOTOP * 5 ;
          R := RR ;
          for J := 1 TO R do begin
            for I := 1 TO N do DER[I,J] := 0.0 ;
            PP[J] := 0.0 ;
            P[J] := 0.0 ;
          end ;
          DERIV(DER,XJO,XJU,XI1,XI2,FO,FU,ISOTOP,N,R ) ;
          MODDER( DER,DER2,R,FIXED_PAR,NFP,REFP,SORT,VALFP,Y,YY) ;
          LEASTS (DER2,DY,YY,N,R,CA,PP,P,CHI,IER ) ;
          if IER = 0 then
            begin
                WRITEOUT(PP,CA,XJO,XJU,XI1,XI2,FO,FU,ISOTOP,Y,
                DY,DER,N,R,RR,CHI,KOPF,COMMENT,SORT,NFP,REFP,VALFP,
                PNAME,XNAME);
                WRITEOUTDAT(PP,CA,XJO,XJU,XI1,XI2,FO,FU,ISOTOP,Y,
                DY,DER,N,R,RR,CHI,KOPF,COMMENT,SORT,NFP,REFP,VALFP,
                PNAME,XNAME);
            end;
       end ;
     end;
     if NOT FLAG then writeln(#7,'Der Fit wird deshalb abgebrochen.') ;
     parameter_speichern;
end.
