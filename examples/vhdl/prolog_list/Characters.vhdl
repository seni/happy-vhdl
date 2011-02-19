library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

use work.prolog_list_types.all;

package characters is

  subtype character_t is natural range 0 to 2**8 - 1;

  constant C_NEW_LINE        : character_t := 10;
  constant C_CARRIAGE_RETURN : character_t := 13;
  constant C_SPACE           : character_t := 32;
  constant C_DOLLAR          : character_t := 36;
  constant C_LEFT_PAREN      : character_t := 40;
  constant C_RIGHT_PAREN     : character_t := 41;
  constant C_MULT            : character_t := 42;
  constant C_PLUS            : character_t := 43;
  constant C_COMMA           : character_t := 44;
  constant C_0               : character_t := 48;
  constant C_1               : character_t := 49;
  constant C_2               : character_t := 50;
  constant C_3               : character_t := 51;
  constant C_4               : character_t := 52;
  constant C_5               : character_t := 53;
  constant C_6               : character_t := 54;
  constant C_7               : character_t := 55;
  constant C_8               : character_t := 56;
  constant C_9               : character_t := 57;
  constant C_UPPER_A         : character_t := 65;
  constant C_UPPER_B         : character_t := 66;
  constant C_UPPER_C         : character_t := 67;
  constant C_UPPER_D         : character_t := 68;
  constant C_UPPER_E         : character_t := 69;
  constant C_UPPER_F         : character_t := 70;
  constant C_UPPER_G         : character_t := 71;
  constant C_UPPER_H         : character_t := 72;
  constant C_UPPER_I         : character_t := 73;
  constant C_UPPER_J         : character_t := 74;
  constant C_UPPER_K         : character_t := 75;
  constant C_UPPER_L         : character_t := 76;
  constant C_UPPER_M         : character_t := 77;
  constant C_UPPER_N         : character_t := 78;
  constant C_UPPER_O         : character_t := 79;
  constant C_UPPER_P         : character_t := 80;
  constant C_UPPER_Q         : character_t := 81;
  constant C_UPPER_R         : character_t := 82;
  constant C_UPPER_S         : character_t := 83;
  constant C_UPPER_T         : character_t := 84;
  constant C_UPPER_U         : character_t := 85;
  constant C_UPPER_V         : character_t := 86;
  constant C_UPPER_W         : character_t := 87;
  constant C_UPPER_X         : character_t := 88;
  constant C_UPPER_Y         : character_t := 89;
  constant C_UPPER_Z         : character_t := 90;
  constant C_OPEN_LIST       : character_t := 91;
  constant C_CLOSE_LIST      : character_t := 93;
  constant C_UNDERSCORE      : character_t := 95;
  constant C_lower_a         : character_t := 97;
  constant C_lower_b         : character_t := 98;
  constant C_lower_c         : character_t := 99;
  constant C_lower_d         : character_t := 100;
  constant C_lower_e         : character_t := 101;
  constant C_lower_f         : character_t := 102;
  constant C_lower_g         : character_t := 103;
  constant C_lower_h         : character_t := 104;
  constant C_lower_i         : character_t := 105;
  constant C_lower_j         : character_t := 106;
  constant C_lower_k         : character_t := 107;
  constant C_lower_l         : character_t := 108;
  constant C_lower_m         : character_t := 109;
  constant C_lower_n         : character_t := 110;
  constant C_lower_o         : character_t := 111;
  constant C_lower_p         : character_t := 112;
  constant C_lower_q         : character_t := 113;
  constant C_lower_r         : character_t := 114;
  constant C_lower_s         : character_t := 115;
  constant C_lower_t         : character_t := 116;
  constant C_lower_u         : character_t := 117;
  constant C_lower_v         : character_t := 118;
  constant C_lower_w         : character_t := 119;
  constant C_lower_x         : character_t := 120;
  constant C_lower_y         : character_t := 121;
  constant C_lower_z         : character_t := 122;
  constant C_HT_SEP          : character_t := 124;

  function c_pos ( char : in character )
    return character_t;

end package characters;

package body characters is

  function c_pos ( char : in character )
    return character_t is
  begin

-- return conv_integer (character'pos (char));

    case char is

      when LF  => return C_NEW_LINE;
      when VT  => return C_CARRIAGE_RETURN;
      when ' ' => return C_SPACE;
      when '$' => return C_DOLLAR;
      when '(' => return C_LEFT_PAREN;
      when ')' => return C_RIGHT_PAREN;
      when '*' => return C_MULT;
      when '+' => return C_PLUS;
      when ',' => return C_COMMA;
      when '0' => return C_0;
      when '1' => return C_1;
      when '2' => return C_2;
      when '3' => return C_3;
      when '4' => return C_4;
      when '5' => return C_5;
      when '6' => return C_6;
      when '7' => return C_7;
      when '8' => return C_8;
      when '9' => return C_9;
      when 'A' => return C_UPPER_A;
      when 'B' => return C_UPPER_B;
      when 'C' => return C_UPPER_C;
      when 'D' => return C_UPPER_D;
      when 'E' => return C_UPPER_E;
      when 'F' => return C_UPPER_F;
      when 'G' => return C_UPPER_G;
      when 'H' => return C_UPPER_H;
      when 'I' => return C_UPPER_I;
      when 'J' => return C_UPPER_J;
      when 'K' => return C_UPPER_K;
      when 'L' => return C_UPPER_L;
      when 'M' => return C_UPPER_M;
      when 'N' => return C_UPPER_N;
      when 'O' => return C_UPPER_O;
      when 'P' => return C_UPPER_P;
      when 'Q' => return C_UPPER_Q;
      when 'R' => return C_UPPER_R;
      when 'S' => return C_UPPER_S;
      when 'T' => return C_UPPER_T;
      when 'U' => return C_UPPER_U;
      when 'V' => return C_UPPER_V;
      when 'W' => return C_UPPER_W;
      when 'X' => return C_UPPER_X;
      when 'Y' => return C_UPPER_Y;
      when 'Z' => return C_UPPER_Z;
      when '[' => return C_OPEN_LIST;
      when ']' => return C_CLOSE_LIST;
      when '_' => return C_UNDERSCORE;
      when 'a' => return C_lower_a;
      when 'b' => return C_lower_b;
      when 'c' => return C_lower_c;
      when 'd' => return C_lower_d;
      when 'e' => return C_lower_e;
      when 'f' => return C_lower_f;
      when 'g' => return C_lower_g;
      when 'h' => return C_lower_h;
      when 'i' => return C_lower_i;
      when 'j' => return C_lower_j;
      when 'k' => return C_lower_k;
      when 'l' => return C_lower_l;
      when 'm' => return C_lower_m;
      when 'n' => return C_lower_n;
      when 'o' => return C_lower_o;
      when 'p' => return C_lower_p;
      when 'q' => return C_lower_q;
      when 'r' => return C_lower_r;
      when 's' => return C_lower_s;
      when 't' => return C_lower_t;
      when 'u' => return C_lower_u;
      when 'v' => return C_lower_v;
      when 'w' => return C_lower_w;
      when 'x' => return C_lower_x;
      when 'y' => return C_lower_y;
      when 'z' => return C_lower_z;
      when '|' => return C_HT_SEP;

      when others => return C_DOLLAR;

    end case;

  end c_pos;

end package body characters;
