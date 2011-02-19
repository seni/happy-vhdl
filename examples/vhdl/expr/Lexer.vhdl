library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;

use work.expr_types.all;
use work.Characters.all;

entity lexer is
  port ( char               : in  character_t;
         terminal           : out terminal_t;
         next_character_req : out std_logic;
         next_terminal_ack  : in  std_logic;
         next_terminal_rdy  : out std_logic;
         lexer_fail         : out std_logic;
         clock, reset       : in  std_logic
  );
end lexer;

architecture lexer_architecture of lexer is

  type state_t is ( start, end_of_file, fail,
                    send_terminal, -- send_terminal2,
                    left_paren,
                    right_paren,
                    plus,
                    mult,
                    num
  );

  -- signals
  signal s_current_state, s_next_state       : state_t;
  signal s_current_terminal, s_next_terminal : terminal_t;
  signal s_next_terminal_ack                 : std_logic;

--  signal char_num                            : character_t;

  impure function new_terminal return state_t is

    variable ret : state_t := fail;

  begin

    if (char = C_SPACE) or
      (char = C_NEW_LINE) or
      (char = C_CARRIAGE_RETURN) then -- eat delimiters
      ret := start;
    end if;

    if char = C_LEFT_PAREN then
      ret := left_paren;
    end if;

    if char = C_RIGHT_PAREN then
      ret := right_paren;
    end if;

    if char = C_PLUS then
      ret := plus;
    end if;

    if char = C_MULT then
      ret := mult;
    end if;

    if (char = C_lower_n)
    then
      ret := num;
    end if;

    if char = C_DOLLAR then
      ret := end_of_file;
    end if;

    return ret;

  end new_terminal;

begin

--  char_num <= integer_conv (character'pos (char));

  terminal <= s_current_terminal;

  lexer : process ( s_current_state, char,
                    s_next_terminal_ack,
                    s_current_terminal
                  )
  begin

    next_character_req <= '1';
    next_terminal_rdy <= '0';
    lexer_fail <= '0';
    s_next_state <= fail;
    s_next_terminal <= s_current_terminal;

    case s_current_state is
      when start =>
        s_next_state <= new_terminal;

      when left_paren =>
        s_next_terminal <= tToken_left_paren;
        s_next_state    <= send_terminal;

      when right_paren =>
        s_next_terminal <= tToken_right_paren;
        s_next_state    <= send_terminal;

      when plus =>
        s_next_terminal <= tToken_plus;
        s_next_state    <= send_terminal;

      when mult =>
        s_next_terminal <= tToken_mult;
        s_next_state    <= send_terminal;

      when num =>
        s_next_terminal <= tToken_num;
        s_next_state    <= send_terminal;

      when end_of_file =>
        s_next_terminal <= eof;
        s_next_state    <= send_terminal;

      when fail =>
        next_character_req <= '0';
        lexer_fail   <= '1';
        s_next_state <= fail;

      when send_terminal =>
        next_character_req <= '0';
        if s_next_terminal_ack = '0' then
          next_terminal_rdy  <= '1';
          s_next_state       <= send_terminal;
        else
--           s_next_state <= send_terminal2;
          s_next_state <= new_terminal;
        end if;

--       when send_terminal2 =>
--         next_terminal_rdy  <= '0';
--         next_character_req <= '0';
--         if s_next_terminal_ack = '1' then
--           s_next_state <= send_terminal2;
--         else
--           next_character_req <= '0';
--           s_next_state <= new_terminal;
--         end if;

      when others =>
        next_character_req <= '0';
        s_next_state <= fail;

    end case;

  end process lexer;

  next_state_update: process ( clock, reset )
  begin
    if (reset ='1') then
      s_current_state         <= start;
      s_current_terminal      <= eof;
      s_next_terminal_ack     <= '0';
    elsif rising_edge (clock) then
      s_current_state         <= s_next_state;
      s_current_terminal      <= s_next_terminal;
      s_next_terminal_ack     <= next_terminal_ack;
    end if;
  end process next_state_update;

end lexer_architecture;
