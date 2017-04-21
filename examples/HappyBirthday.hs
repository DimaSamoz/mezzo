
import Mezzo

mel = melody :~| r :~| r :<. g :<< g :^ a :| g :| c' :> b
             :<. g :<< g :^ a :| g :| d' :> c'
             :<. g :<< g :^ g' :| e' :| c' :| b :| a
             :<. f' :<< f' :^ e' :| c' :| d' :>. c'

chords = ph_IVI (ton_T_T ton ton) (dom_D_D dom_V dom_V) (ton_T_T ton ton)
      :+ cadence (full subdom_ii auth_V)

sco = score setKeySig c_maj
            setTimeSig triple
            setTitle "Happy Birthday Song"
            withMusic (melAccomp mel chords)

comp = renderScore "examples/HappyBirthday.mid" sco
