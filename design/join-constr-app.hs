
==================== [12] After Simplifier ====================
  

-- RHS size: {terms: 3, types: 5, coercions: 0, joins: 0/0}
lvl_sluQ
  = \ @ b_Xl2K ->
      patError
        "src/Streamly/Internal/Data/Stream/StreamD.hs:(1011,9)-(1022,37)|case"#

-- RHS size: {terms: 4, types: 8, coercions: 0, joins: 0/0}
lvl_sluM = \ @ a1_alfS -> ([], [])

Rec {
-- RHS size: {terms: 38, types: 49, coercions: 0, joins: 0/1}
splitAt'_sluL
  = \ @ a1_alfS ds5_alfT ds6_alfU ->
      case ds6_alfU of {
        [] -> lvl_sluM;
        : ipv_alfY ipv1_alfZ ->
          case ds5_alfT of { I# ds7_alg3 ->
          case ds7_alg3 of ds8_alg5 {
            __DEFAULT ->
              let { ds9_sltM = splitAt'_sluL (I# (-# ds8_alg5 1#)) ipv1_alfZ } in
              (: ipv_alfY
                 (case ds9_sltM of { (xs'_alg9, xs''_alga) -> xs'_alg9 }),
               case ds9_sltM of { (xs'_alge, xs''_algf) -> xs''_algf });
            1# -> (: ipv_alfY [], ipv1_alfZ)
          }
          }
      }
end Rec }

-- RHS size: {terms: 3, types: 5, coercions: 0, joins: 0/0}
lvl_sluF
  = \ @ b_Xl2K ->
      patError
        "src/Streamly/Internal/Data/Stream/StreamD.hs:(993,17)-(1004,45)|case"#

-- RHS size: {terms: 4, types: 8, coercions: 0, joins: 0/0}
lvl_sluB = \ @ a1_aleu -> ([], [])

Rec {
-- RHS size: {terms: 38, types: 49, coercions: 0, joins: 0/1}
splitAt'_sluA
  = \ @ a1_aleu ds4_alev ds5_alew ->
      case ds5_alew of {
        [] -> lvl_sluB;
        : ipv_aleA ipv1_aleB ->
          case ds4_alev of { I# ds6_aleF ->
          case ds6_aleF of ds7_aleH {
            __DEFAULT ->
              let { ds8_sltG = splitAt'_sluA (I# (-# ds7_aleH 1#)) ipv1_aleB } in
              (: ipv_aleA
                 (case ds8_sltG of { (xs'_aleL, xs''_aleM) -> xs'_aleL }),
               case ds8_sltG of { (xs'_aleQ, xs''_aleR) -> xs''_aleR });
            1# -> (: ipv_aleA [], ipv1_aleB)
          }
          }
      }
end Rec }

-- RHS size: {terms: 9, types: 12, coercions: 0, joins: 0/0}
lvl_sluv
  = \ @ a_Xl2I ->
      State
        Nothing
        Nothing
        defaultMaxThreads
        defaultMaxThreads
        Nothing
        Nothing
        False

-- RHS size: {terms: 33, types: 24, coercions: 4, joins: 0/0}
lvl_sluT
  = \ @ b_Xl2K ->
      error
        ((PushCallStack
            (build (\ @ b_akVp -> unpackFoldrCString# "error"#))
            (SrcLoc
               (build
                  (\ @ b_akVp -> unpackFoldrCString# "streamly-0.7.1-inplace"#))
               (build
                  (\ @ b_akVp ->
                     unpackFoldrCString# "Streamly.Internal.Data.Stream.StreamD"#))
               (build
                  (\ @ b_akVp ->
                     unpackFoldrCString#
                       "src/Streamly/Internal/Data/Stream/StreamD.hs"#))
               (I# 980#)
               (I# 47#)
               (I# 980#)
               (I# 59#))
            EmptyCallStack)
         `cast` <Co:4>)
        (build (\ @ b_akVp -> unpackFoldrCString# "init"#))

-- RHS size: {terms: 240, types: 305, coercions: 14, joins: 2/4}
$sparselMx'_slAa
  = \ @ s_Xl2G
      @ a_Xl2I
      @ b_Xl2K
      pstep_al2G
      initial_al2H
      ds_al2I
      eta_B1 ->
      case ds_al2I of { UnStream @ s1_al2V step1_al2W state_al3e ->
      joinrec {
        go_slzX ds1_al3x st_al3y buf_al3z pst_al3A rval_al3B eta_X1n
          = case ds1_al3x of { __DEFAULT ->
            case pst_al3A of pst1_al3J { __DEFAULT ->
            case ((step1_al2W lvl_sluv st_al3y) `cast` <Co:4>) eta_X1n of
            { (# ipv_akT1, ipv1_akT2 #) ->
            case ipv1_akT2 of {
              Yield x_ale5 s2_ale6 ->
                case ((pstep_al2G pst1_al3J x_ale5) `cast` <Co:4>) ipv_akT1 of
                { (# ipv_XkVL, ipv1_XkVN #) ->
                case ipv1_XkVN of {
                  Yield n_aleb pst2_alec b1_aled ->
                    jump go_slzX
                      SPEC
                      s2_ale6
                      (case n_aleb of { I# y_alej ->
                       case <# 0# y_alej of {
                         __DEFAULT -> [];
                         1# -> $wunsafeTake y_alej (: x_ale5 buf_al3z)
                       }
                       })
                      pst2_alec
                      b1_aled
                      ipv_XkVL;
                  Skip n_aleo pst2_alep ->
                    let {
                      ds3_sltH
                        = case n_aleo of wild3_aleT { I# x1_aleV ->
                          case <=# x1_aleV 0# of {
                            __DEFAULT -> splitAt'_sluA wild3_aleT (: x_ale5 buf_al3z);
                            1# -> ([], : x_ale5 buf_al3z)
                          }
                          } } in
                    jump gobuf_slA8
                      SPEC
                      s2_ale6
                      (case ds3_sltH of { (src0_alf0, buf1_alf1) -> buf1_alf1 })
                      (case ds3_sltH of { (src0_alf6, buf1_alf7) ->
                       reverse1 src0_alf6 []
                       })
                      pst2_alep
                      rval_al3B
                      ipv_XkVL;
                  Stop ds3_alfa b1_alfb -> (# ipv_XkVL, b1_alfb #);
                  Error ipv_alsZ -> case lvl_sluF of wild_00 { }
                }
                };
              Skip s2_alff ->
                jump go_slzX SPEC s2_alff buf_al3z pst1_al3J rval_al3B ipv_akT1;
              Stop -> (# ipv_akT1, rval_al3B #)
            }
            }
            }
            };
        gobuf_slA8 ds1_alfj
                   s2_alfk
                   buf_alfl
                   ds2_alfm
                   pst_alfn
                   rval_alfo
                   eta_X1v
          = case ds1_alfj of { __DEFAULT ->
            case ds2_alfm of {
              [] ->
                case pst_alfn of pst1_alft { __DEFAULT ->
                jump go_slzX SPEC s2_alfk buf_alfl pst1_alft rval_alfo eta_X1v
                };
              : x_alfv xs_alfw ->
                case pst_alfn of pst1_alfy { __DEFAULT ->
                case ((pstep_al2G pst1_alfy x_alfv) `cast` <Co:4>) eta_X1v of
                { (# ipv_akT1, ipv1_akT2 #) ->
                case ipv1_akT2 of {
                  Yield n_alfC pst2_alfD b1_alfE ->
                    jump gobuf_slA8
                      SPEC
                      s2_alfk
                      (case n_alfC of { I# y_alfI ->
                       case <# 0# y_alfI of {
                         __DEFAULT -> [];
                         1# -> $wunsafeTake y_alfI (: x_alfv buf_alfl)
                       }
                       })
                      xs_alfw
                      pst2_alfD
                      b1_alfE
                      ipv_akT1;
                  Skip n_alfM pst2_alfN ->
                    let {
                      ds4_sltN
                        = case n_alfM of wild3_algh { I# x1_algj ->
                          case <=# x1_algj 0# of {
                            __DEFAULT -> splitAt'_sluL wild3_algh (: x_alfv buf_alfl);
                            1# -> ([], : x_alfv buf_alfl)
                          }
                          } } in
                    jump gobuf_slA8
                      SPEC
                      s2_alfk
                      (case ds4_sltN of { (src0_algo, buf1_algp) -> buf1_algp })
                      (case ds4_sltN of { (src0_algt, buf1_algu) ->
                       augment
                         (\ @ b_alwx c_alwy n_alwz ->
                            foldr c_alwy n_alwz (reverse1 src0_algt []))
                         xs_alfw
                       })
                      pst2_alfN
                      rval_alfo
                      ipv_akT1;
                  Stop ds4_algx b1_algy -> (# ipv_akT1, b1_algy #);
                  Error ipv_alt2 -> case lvl_sluQ of wild_00 { }
                }
                }
                }
            }
            }; } in
      case (initial_al2H `cast` <Co:2>) eta_B1 of
      { (# ipv_akT1, ipv1_akT2 #) ->
      jump go_slzX SPEC state_al3e [] ipv1_akT2 lvl_sluT ipv_akT1
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 26, joins: 0/0}
$sparselMx'_slt6 = $sparselMx'_slAa `cast` <Co:26>

-- RHS size: {terms: 5, types: 19, coercions: 0, joins: 0/0}
lvl_slym = \ @ a_XlgV s_XkYp -> (# s_XkYp, Stop #)

-- RHS size: {terms: 7, types: 19, coercions: 0, joins: 0/0}
lvl_sltV = \ @ a_XlgV @ r_alh0 _ _ _ stp_alh4 -> stp_alh4

-- RHS size: {terms: 8, types: 21, coercions: 4, joins: 0/0}
lvl_slyn
  = \ @ a_XlgV a1_algZ s_XkWv ->
      (# s_XkWv, Yield a1_algZ (lvl_sltV `cast` <Co:4>) #)

-- RHS size: {terms: 9, types: 23, coercions: 0, joins: 0/0}
lvl_slyo
  = \ @ a_XliX a1_algX x_algY s_XkWA ->
      (# s_XkWA, Yield a1_algX x_algY #)

-- RHS size: {terms: 8, types: 17, coercions: 32, joins: 0/0}
lvl_sltY
  = \ @ a_Xlj2 gst_algV m1_algW ->
      (m1_algW `cast` <Co:3>)
        gst_algV
        (lvl_slyo `cast` <Co:13>)
        (lvl_slyn `cast` <Co:9>)
        (lvl_slym `cast` <Co:7>)

-- RHS size: {terms: 3, types: 8, coercions: 0, joins: 0/0}
$sfromStreamK_sltO = \ @ a_Xlj3 -> UnStream lvl_sltY

-- RHS size: {terms: 65, types: 153, coercions: 38, joins: 1/3}
$stoStreamK_slAN
  = \ @ a_XlqM ds_alqM @ r_altS eta_B5 eta_B4 eta_B3 eta_B2 eta_B1 ->
      case ds_alqM of { UnStream @ s_alqQ step1_alr0 state_alr1 ->
      letrec {
        go_slAM
          = \ st_alqS @ r_alqT st1_alqU yld_alqV _ stp_alqX eta_X1C ->
              let {
                lvl_sltZ
                  = case st1_alqU of
                    { State ds1_alr5 ds2_alrc ds3_alrg ds4_alrn ds5_alro ds6_alrs
                            ds7_alrw ->
                    State Nothing Nothing ds3_alrg ds4_alrn ds5_alro ds6_alrs ds7_alrw
                    } } in
              joinrec {
                go'_slyt ss_alqZ s_akSY
                  = case ((step1_alr0 lvl_sltZ ss_alqZ) `cast` <Co:4>) s_akSY of
                    { (# ipv_akT1, ipv1_akT2 #) ->
                    case ipv1_akT2 of {
                      Yield x_alrB s1_alrC ->
                        ((yld_alqV x_alrB ((go_slAM s1_alrC) `cast` <Co:30>))
                         `cast` <Co:2>)
                          ipv_akT1;
                      Skip s1_alrF -> jump go'_slyt s1_alrF ipv_akT1;
                      Stop -> (stp_alqX `cast` <Co:2>) ipv_akT1
                    }
                    }; } in
              jump go'_slyt st_alqS eta_X1C; } in
      go_slAM state_alr1 eta_B5 eta_B4 eta_B3 eta_B2 eta_B1
      }

-- RHS size: {terms: 1, types: 0, coercions: 37, joins: 0/0}
$stoStreamK_slsw = $stoStreamK_slAN `cast` <Co:37>

-- RHS size: {terms: 32, types: 90, coercions: 17, joins: 0/2}
$sunfoldrM_slsT
  = \ @ s_XlsH @ a_XlsK next_alqq state_alqr ->
      let {
        step_slyD
          = \ @ p_alqv _ eta1_alqx s_akSY ->
              case ((next_alqq eta1_alqx) `cast` <Co:5>) s_akSY of
              { (# ipv_akT1, ipv1_akT2 #) ->
              (# ipv_akT1,
                 case ipv1_akT2 of {
                   Nothing -> Stop;
                   Just ds_alqC ->
                     case ds_alqC of { (x_alqG, s1_alqH) -> Yield x_alqG s1_alqH }
                 } #)
              } } in
      let { step_slsV = step_slyD `cast` <Co:12> } in
      UnStream step_slsV state_alqr

-- RHS size: {terms: 44, types: 108, coercions: 39, joins: 0/1}
$sunfoldrM_slUd
  = \ @ b_Xlqb
      @ a_Xlqd
      step_alqb
      seed_alqc
      @ r_altS
      eta_B4
      eta_B3
      eta_B2
      eta_B1
      eta_X2 ->
      letrec {
        go_slUc
          = \ st_alqS @ r_alqT _ yld_alqV _ stp_alqX eta_X1g ->
              case ((step_alqb st_alqS) `cast` <Co:5>) eta_X1g of
              { (# ipv_akT1, ipv1_akT2 #) ->
              case ipv1_akT2 of {
                Nothing -> (stp_alqX `cast` <Co:2>) ipv_akT1;
                Just ds1_alsI ->
                  case ds1_alsI of { (x_alsM, s1_alsN) ->
                  ((yld_alqV x_alsM ((go_slUc s1_alsN) `cast` <Co:30>))
                   `cast` <Co:2>)
                    ipv_akT1
                  }
              }
              }; } in
      go_slUc seed_alqc eta_B4 eta_B3 eta_B2 eta_B1 eta_X2

-- RHS size: {terms: 1, types: 0, coercions: 50, joins: 0/0}
$sunfoldrM_sltP = $sunfoldrM_slUd `cast` <Co:50>

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$sunfoldrMSerial_sltQ = $sunfoldrM_sltP

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule_skSD = "main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule_skSE = TrNameS $trModule_skSD

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
$trModule_skSF = "Main"#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
$trModule_skSG = TrNameS $trModule_skSF

-- RHS size: {terms: 3, types: 0, coercions: 0, joins: 0/0}
$trModule = Module $trModule_skSE $trModule_skSG

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_slva = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_slvc = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_slve = I# 0#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_slv4 = I# 1#

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
lvl_slv5 = I# 1#

-- RHS size: {terms: 3, types: 2, coercions: 0, joins: 0/0}
lvl_slv6 = (lvl_slv4, lvl_slv5)

-- RHS size: {terms: 509, types: 741, coercions: 26, joins: 2/12}
main_sli7
  = \ s_akSY ->
      case parseCLIOpts1 defaultStreamSize s_akSY of
      { (# ipv_akT1, ipv1_akT2 #) ->
      case ipv1_akT2 of { (value_aeNm, cfg_aeNn, benches_aeNo) ->
      let {
        ds_slv9
          = case value_aeNm of { I# ww1_akrv ->
            I# (uncheckedIShiftRA# ww1_akrv 1#)
            } } in
      case value_aeNm of { I# ipv_skVl ->
      case cfg_aeNn of wild_akcW
      { Config ds1_akcZ ds2_akd0 ds3_akd1 ds4_akd2 ds5_akd3 ds6_akd4
               ds7_akd5 ds8_akd6 ds9_akd7 ds10_akd8 ds11_akd9 ds12_akda ds13_akdb
               ds14_akdc ds15_akdd ds16_akde ds17_akdf ds18_akdg ds19_akdh
               ds20_akdj ds21_akdk ds22_akdl ds23_akdm ->
      defaultMain3
        ds22_akdl
        wild_akcW
        benches_aeNo
        (: (BenchGroup
              (build (\ @ b_akVp -> unpackFoldrCString# "o1"#))
              (: (BenchGroup
                    (build (\ @ b_akVp -> unpackFoldrCString# "parser"#))
                    (: (Benchmark
                          (build (\ @ b_akVp -> unpackFoldrCString# "split (all,any)"#))
                          (letrec {
                             go_slpX
                               = \ n_akbO eta_X2d ->
                                   case n_akbO of { I64# x_akUC ->
                                   case <=# x_akUC 0# of {
                                     __DEFAULT ->
                                       case $fRandomInt3 lvl_slv6 eta_X2d of
                                       { (# ipv_XkUa, ipv1_XkUc #) ->
                                       letrec {
                                         gobuf_slKI
                                           = \ ds1_alfj
                                               s2_alfk
                                               buf_alfl
                                               ds2_alfm
                                               pst_alfn
                                               rval_alfo
                                               eta_X2F ->
                                               case ds1_alfj of { __DEFAULT ->
                                               case ds2_alfm of {
                                                 [] ->
                                                   case pst_alfn of pst1_alft { __DEFAULT ->
                                                   go_slKj
                                                     SPEC
                                                     s2_alfk
                                                     buf_alfl
                                                     pst1_alft
                                                     rval_alfo
                                                     eta_X2F
                                                   };
                                                 : x_alfv xs_alfw ->
                                                   case pst_alfn of {
                                                     SeqParseL st_akgH ->
                                                       join {
                                                         $j_sm2u n_alfM pst2_alfN
                                                           = let {
                                                               ds4_alfP
                                                                 = letrec {
                                                                     splitAt'_alfQ
                                                                       = \ @ a1_alfS
                                                                           ds5_alfT
                                                                           ds6_alfU ->
                                                                           case ds6_alfU of {
                                                                             [] -> ([], []);
                                                                             : ipv_alfY ipv1_alfZ ->
                                                                               case ds5_alfT of
                                                                               { I# ds7_alg3 ->
                                                                               case ds7_alg3
                                                                               of ds8_alg5 {
                                                                                 __DEFAULT ->
                                                                                   let {
                                                                                     ds9_alg6
                                                                                       = splitAt'_alfQ
                                                                                           (I#
                                                                                              (-#
                                                                                                 ds8_alg5
                                                                                                 1#))
                                                                                           ipv1_alfZ } in
                                                                                   (: ipv_alfY
                                                                                      (case ds9_alg6
                                                                                       of
                                                                                       { (xs'_alg9,
                                                                                          xs''_alga) ->
                                                                                       xs'_alg9
                                                                                       }),
                                                                                    case ds9_alg6 of
                                                                                    { (xs'_alge,
                                                                                       xs''_algf) ->
                                                                                    xs''_algf
                                                                                    });
                                                                                 1# ->
                                                                                   (: ipv_alfY [],
                                                                                    ipv1_alfZ)
                                                                               }
                                                                               }
                                                                           }; } in
                                                                   case n_alfM of wild3_algh
                                                                   { I# x1_algj ->
                                                                   case <=# x1_algj 0# of {
                                                                     __DEFAULT ->
                                                                       splitAt'_alfQ
                                                                         wild3_algh
                                                                         (: x_alfv buf_alfl);
                                                                     1# -> ([], : x_alfv buf_alfl)
                                                                   }
                                                                   } } in
                                                             gobuf_slKI
                                                               SPEC
                                                               s2_alfk
                                                               (case ds4_alfP of
                                                                { (src0_algo, buf1_algp) ->
                                                                buf1_algp
                                                                })
                                                               (case ds4_alfP of
                                                                { (src0_algt, buf1_algu) ->
                                                                augment
                                                                  (\ @ b_alwx c_alwy n_alwz ->
                                                                     foldr
                                                                       c_alwy
                                                                       n_alwz
                                                                       (reverse1 src0_algt []))
                                                                  xs_alfw
                                                                })
                                                               pst2_alfN
                                                               rval_alfo
                                                               eta_X2F } in
                                                       case st_akgH of {
                                                         False ->
                                                           jump $j_sm2u
                                                             lvl_slva (SeqParseR ((,) False) False);
                                                         True ->
                                                           case x_alfv of { I# x_akrH ->
                                                           case ds_slv9 of { I# y_akrL ->
                                                           case <=# x_akrH y_akrL of {
                                                             __DEFAULT ->
                                                               jump $j_sm2u
                                                                 lvl_slvc
                                                                 (SeqParseR ((,) False) False);
                                                             1# ->
                                                               jump $j_sm2u
                                                                 lvl_slve (SeqParseL True)
                                                           }
                                                           }
                                                           }
                                                       };
                                                     SeqParseR f_akh3 st_akh4 ->
                                                       case st_akh4 of {
                                                         False ->
                                                           case x_alfv of { I# x_akfr ->
                                                           case ># x_akfr ipv_skVl of {
                                                             __DEFAULT ->
                                                               gobuf_slKI
                                                                 SPEC
                                                                 s2_alfk
                                                                 []
                                                                 xs_alfw
                                                                 (SeqParseR f_akh3 False)
                                                                 (f_akh3 False)
                                                                 eta_X2F;
                                                             1# -> (# eta_X2F, f_akh3 True #)
                                                           }
                                                           };
                                                         True -> (# eta_X2F, f_akh3 True #)
                                                       }
                                                   }
                                               }
                                               };
                                         go_slKj
                                           = \ ds1_al3x
                                               st_al3y
                                               buf_al3z
                                               pst_al3A
                                               rval_al3B
                                               eta_X2x ->
                                               case ds1_al3x of { __DEFAULT ->
                                               case pst_al3A of pst1_al3J { __DEFAULT ->
                                               case ipv1_XkUc of { I# x_akfI ->
                                               case st_al3y of wild_akfp { I# x_akfr ->
                                               case ># x_akfr (+# x_akfI ipv_skVl) of {
                                                 __DEFAULT ->
                                                   case pst1_al3J of {
                                                     SeqParseL st_akgH ->
                                                       join {
                                                         $j_sm9M n_aleo pst2_alep
                                                           = let {
                                                               ds3_aler
                                                                 = letrec {
                                                                     splitAt'_ales
                                                                       = \ @ a1_aleu
                                                                           ds4_alev
                                                                           ds5_alew ->
                                                                           case ds5_alew of {
                                                                             [] -> ([], []);
                                                                             : ipv_aleA ipv1_aleB ->
                                                                               case ds4_alev of
                                                                               { I# ds6_aleF ->
                                                                               case ds6_aleF
                                                                               of ds7_aleH {
                                                                                 __DEFAULT ->
                                                                                   let {
                                                                                     ds8_aleI
                                                                                       = splitAt'_ales
                                                                                           (I#
                                                                                              (-#
                                                                                                 ds7_aleH
                                                                                                 1#))
                                                                                           ipv1_aleB } in
                                                                                   (: ipv_aleA
                                                                                      (case ds8_aleI
                                                                                       of
                                                                                       { (xs'_aleL,
                                                                                          xs''_aleM) ->
                                                                                       xs'_aleL
                                                                                       }),
                                                                                    case ds8_aleI of
                                                                                    { (xs'_aleQ,
                                                                                       xs''_aleR) ->
                                                                                    xs''_aleR
                                                                                    });
                                                                                 1# ->
                                                                                   (: ipv_aleA [],
                                                                                    ipv1_aleB)
                                                                               }
                                                                               }
                                                                           }; } in
                                                                   case n_aleo of wild3_aleT
                                                                   { I# x1_aleV ->
                                                                   case <=# x1_aleV 0# of {
                                                                     __DEFAULT ->
                                                                       splitAt'_ales
                                                                         wild3_aleT
                                                                         (: wild_akfp buf_al3z);
                                                                     1# ->
                                                                       ([], : wild_akfp buf_al3z)
                                                                   }
                                                                   } } in
                                                             gobuf_slKI
                                                               SPEC
                                                               (I# (+# x_akfr 1#))
                                                               (case ds3_aler of
                                                                { (src0_alf0, buf1_alf1) ->
                                                                buf1_alf1
                                                                })
                                                               (case ds3_aler of
                                                                { (src0_alf6, buf1_alf7) ->
                                                                reverse1 src0_alf6 []
                                                                })
                                                               pst2_alep
                                                               rval_al3B
                                                               eta_X2x } in
                                                       case st_akgH of {
                                                         False ->
                                                           jump $j_sm9M
                                                             lvl_slva (SeqParseR ((,) False) False);
                                                         True ->
                                                           case ds_slv9 of { I# y_akrL ->
                                                           case <=# x_akfr y_akrL of {
                                                             __DEFAULT ->
                                                               jump $j_sm9M
                                                                 lvl_slvc
                                                                 (SeqParseR ((,) False) False);
                                                             1# ->
                                                               jump $j_sm9M
                                                                 lvl_slve (SeqParseL True)
                                                           }
                                                           }
                                                       };
                                                     SeqParseR f_akh3 st_akh4 ->
                                                       case st_akh4 of {
                                                         False ->
                                                           case ># x_akfr ipv_skVl of {
                                                             __DEFAULT ->
                                                               go_slKj
                                                                 SPEC
                                                                 (I# (+# x_akfr 1#))
                                                                 []
                                                                 (SeqParseR f_akh3 False)
                                                                 (f_akh3 False)
                                                                 eta_X2x;
                                                             1# -> (# eta_X2x, f_akh3 True #)
                                                           };
                                                         True -> (# eta_X2x, f_akh3 True #)
                                                       }
                                                   };
                                                 1# -> (# eta_X2x, rval_al3B #)
                                               }
                                               }
                                               }
                                               }
                                               }; } in
                                       case go_slKj
                                              SPEC
                                              ipv1_XkUc
                                              []
                                              (SeqParseL True)
                                              (error
                                                 ((PushCallStack
                                                     (build
                                                        (\ @ b_akVp ->
                                                           unpackFoldrCString# "error"#))
                                                     (SrcLoc
                                                        (build
                                                           (\ @ b_akVp ->
                                                              unpackFoldrCString#
                                                                "streamly-0.7.1-inplace"#))
                                                        (build
                                                           (\ @ b_akVp ->
                                                              unpackFoldrCString#
                                                                "Streamly.Internal.Data.Stream.StreamD"#))
                                                        (build
                                                           (\ @ b_akVp ->
                                                              unpackFoldrCString#
                                                                "src/Streamly/Internal/Data/Stream/StreamD.hs"#))
                                                        (I# 980#)
                                                        (I# 47#)
                                                        (I# 980#)
                                                        (I# 59#))
                                                     EmptyCallStack)
                                                  `cast` <Co:4>)
                                                 (build
                                                    (\ @ b_akVp -> unpackFoldrCString# "init"#)))
                                              ipv_XkUa
                                       of
                                       { (# ipv_akbY, ipv1_akbZ #) ->
                                       case seq#
                                              (case ipv1_akbZ of { (x_akV5, y_akV6) ->
                                               case x_akV5 of { __DEFAULT ->
                                               case y_akV6 of { __DEFAULT -> () }
                                               }
                                               })
                                              ipv_akbY
                                       of
                                       { (# ipv2_akc3, ipv3_akc4 #) ->
                                       go_slpX (I64# (-# x_akUC 1#)) ipv2_akc3
                                       }
                                       }
                                       };
                                     1# -> (# eta_X2d, () #)
                                   }
                                   }; } in
                           Benchmarkable
                             ((\ ds_akce -> ds_akce) `cast` <Co:3>)
                             ((\ _ eta_X2e -> (# eta_X2e, () #)) `cast` <Co:5>)
                             ((\ _ _ eta_X4t -> (# eta_X4t, () #)) `cast` <Co:7>)
                             ((\ _ eta_X2f eta_X4t -> go_slpX eta_X2f eta_X4t) `cast` <Co:7>)
                             False))
                       []))
                 []))
           [])
        ipv_akT1
      }
      }
      }
      }

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
main = main_sli7 `cast` <Co:3>

-- RHS size: {terms: 2, types: 1, coercions: 3, joins: 0/0}
main_slpm = runMainIO1 (main_sli7 `cast` <Co:3>)

-- RHS size: {terms: 1, types: 0, coercions: 3, joins: 0/0}
main = main_slpm `cast` <Co:3>


------ Local rules for imported ids --------
"SPEC/Main fromStreamK @ IO _" [0]
    forall @ a_algT $dMonad_slq2.
      fromStreamK $dMonad_slq2
      = $sfromStreamK_sltO
"SPEC/Main toStreamK @ IO _" [0]
    forall @ a_alqK $dMonad_slrP.
      toStreamK $dMonad_slrP
      = $stoStreamK_slsw
"SPEC/Main unfoldrM @ IO _ _" [1]
    forall @ s_alqn @ a_alqo $dMonad_slsS.
      unfoldrM $dMonad_slsS
      = $sunfoldrM_slsT
"SPEC/Main unfoldrM @ SerialT @ IO _ _"
    forall @ b_alq7 @ a_alq8 $dMonad_Xltd $dIsStream_slqk.
      unfoldrM $dIsStream_slqk $dMonad_Xltd
      = $sunfoldrM_sltP
"SPEC/Main unfoldrMSerial @ IO _ _" [2]
    forall @ b_alha @ a_alhb $d(%,,%)_slq4.
      unfoldrMSerial $d(%,,%)_slq4
      = $sunfoldrMSerial_sltQ
"SPEC/Main parselMx' @ IO _ _ _" [1]
    forall @ s_al2C @ a_al2D @ b_al2E $dMonad_slt5.
      parselMx' $dMonad_slt5
      = $sparselMx'_slt6

