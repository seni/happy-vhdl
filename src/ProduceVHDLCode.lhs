-----------------------------------------------------------------------------
$Id$

The code generator.

(c) 1993-2001 Andy Gill, Simon Marlow
-----------------------------------------------------------------------------

> module ProduceVHDLCode (produceVHDLParser) where

import

> import Paths_happy		( version )
> import Data.Version		( showVersion )
> import Grammar
> import Target			( Target(..) )
> import GenUtils		( str, nl,
>                                 maybestr,
>                                 fst4, snd4)

> import Maybe 			( isJust, isNothing )
> import Char
> import List

> import Control.Monad.ST
> import Array              ( Array )
> import Data.Array.ST      ( STUArray )
> import Data.Array.Unboxed ( UArray )
> import Data.Array.MArray
> import Data.Array.IArray

> import Debug.Trace

%-----------------------------------------------------------------------------
Produce the complete output file.

> produceVHDLParser :: Grammar 			-- grammar info
>                   -> ActionTable		-- action table
>                   -> GotoTable 		-- goto table
>                   -> FilePath
>                   -> [(FilePath, String)]

> produceVHDLParser g@(Grammar
>                    { lookupProdNo = lookupProd
>                    , token_names = names
>                    , first_nonterm = first_nonterm
>                    , eof_term = eof
>                    , first_term = first_term
>                    , token_specs = token_rep
>                    , starts = starts
>                    })
>	 	action goto filename

Just one start symbol for now

>  = if length starts > 1 then
>    error ("you have defined multiple starting symbols using the %name "
>           ++ "directive\n but VHDL Codegeneration supports only one"
>           ++ "start symbol for now")
>    else [(base_filename ++ "_Parser.vhdl",
>      (  ieee_library_str . nl
>       . nl
>       . str "use work." . str base_filename_low . str "_types.all;" . nl
>       . nl
>       . entity_str . nl
>       . architecture_str . nl
>      ) ""),

>     (base_filename ++ "_Stack.vhdl", stack_file_str
>      base_filename_low . nl $ ""),
>     (base_filename ++ "_Types.vhdl", types_file_str
>      base_filename_low
>      states terminals' nonterminals' . nl $ ""),
>     (base_filename ++ "_Toplevel.vhdl", toplevel_file_str
>      base_filename_low [] . nl $ "")]
>     where

>       base_filename = takeWhile (/= '.') filename
>       base_filename_low = map toLower base_filename

>       states = map (\n -> (n, "state" ++ show n))
>                  [state_first .. state_last]
>           where (state_first, state_last) = bounds action

>       terminals' = map (\(num, t) -> (num, 't' : t)) token_rep
>                    ++ [(eof, "eof")]
>       nonterminals' = (map (\(num, n) -> (num, 'n' : n)) $
>                        slice first_nonterm (first_term - 1) $
>                        assocs names)

>       lookup_state n = states !! 1

>       slice first last list =   drop (first - 1)
>                               $ take last list

>       state_type_decl_str =
>           str "  signal s_current_state,"
>         . str " s_next_state : state_t;" .nl

>       states_case_str =
>           str "    case s_current_state is" . nl
>         . str "      when start =>" . nl
>         . str "        -- shift first state" . nl
>         . str "        s_push <= '1';" . nl
>         . str "        s_push_data <= state0;" . nl
>         . str "        s_next_state <= state0;" . nl
>         . nl
>         . str "      when fail =>" . nl
>         . str "        parser_fail <= '1';" . nl
>         . nl
>         . str "      when update_reduction =>" . nl
>         . str "       s_next_reduce <= '1';" . nl
>         . str "       s_next_state  <= s_pop_data;" . nl

         . str "        s_next_state <= fail;" . nl

>         . (foldl (.) nl $ map
>            (\state ->
>             state_str state)
>            states) . nl
>         . str "    end case;" . nl
>           where state_str state =
>                     str "      when "
>                   . str (snd state) . str " =>" . nl

>                   . str "        if s_current_reduce = '0' then" . nl
>                   . str "          if next_terminal_rdy = '1' then" . nl
>                   . str "            case terminal is"
>                   . (foldl (.) nl $ map action_str (nonterminals'
>                                                     ++ terminals'))
>                   . nl
>                   . str "              when others =>" . nl
>                   . str "                s_next_state <= fail;" . nl . nl
>                   . str "            end case;" . nl
>                   . str "          else" . nl
>                   . str "            s_next_state <= "
>                         . str (snd state) . str ";" . nl
>                   . str "          end if;" . nl
>                   . str "        else" . nl
>                   . str "          -- goto" . nl
>                   . str "          "
>                   . str "case s_current_nt is"
>                   . (foldl (.) nl $ (map goto_str . assocs)
>                                (goto ! fst state))
>                   . str "          "
>                   . str "  when others =>" . nl
>                   . str "          "
>                   . str "    s_next_state <= fail;" . nl . nl
>                   . str "          "
>                   . str "end case;" . nl
>                   . str "        end if;" . nl

>                   . str "      -- end "
>                   . str (snd state) . nl
>                   . nl
>                       where action_str terminal =
>                               let lraction = action
>                                              ! (fst state)
>                                              ! (fst terminal)

                                    Decide on conflicts

>                                   lraction' = case lraction of
>                                               (LR'Multiple _
>                                                s@(LR'Shift _ _))  -> s
>                                               (LR'Multiple _
>                                                r@(LR'Reduce _ _)) -> r
>                                               a                   -> a
>                               in

>                                 str ""
>                               . case lraction' of

>                                   (LR'Shift next_state _) ->
>                                       str "              when "
>                                     . (str $ snd terminal)

>                                     . str " =>" . nl
>                                     . str "                "
>                                     . str "-- shift" . nl
>                                     . str "                "
>                                     . str "s_push <= '1';" . nl
>                                     . str "                "
>                                     . str "s_push_data <= state"
>                                     . str (map toLower $ show next_state)
>                                     . str ";" . nl
>                                     . nl
>                                     . str "                "
>                                     . str "s_next_state <= "
>                                     . str "state"
>                                     . str (map toLower $ show next_state)
>                                     . str ";" . nl
>                                     . str "                "
>                                     . str "next_terminal_ack"
>                                     . str " <= '1';" . nl . nl

>                                   (LR'Reduce rule _) ->
>                                       str "              when "
>                                     . (str $ snd terminal)

>                                     . str " =>" . nl
>                                     . str "                "
>                                     . str "-- reduce" . nl
>                                     . str "                "
>                                     . str "s_pop <= '1';" . nl
>                                     . str "                "
>                                     . str "s_popn <= "
>                                     . str (show
>                                            $ length
>                                            $ snd4
>                                            $ lookupProd rule)
>                                     . str ";" . nl
>                                     . str "                "
>                                     . str "s_next_nt <= "
>                                     . str (snd $ nonterminals' !!
>                                            (fst4 (lookupProd rule) -
>                                             first_nonterm))
>                                     . str ";" . nl
>                                     . str "                "
>                                     . str "s_next_state <= "
>                                     . str "update_reduction;" . nl . nl

>                                   (LR'Accept) ->
>                                       str "              when "
>                                     . (str $ snd terminal)

>                                     . str " =>" . nl
>                                     . str "                "
>                                     . str "-- accept" . nl
>                                     . str "                "
>                                     . str "parser_accept <= '1';" . nl
>                                     . str "                "
>                                     . str "s_next_state <= "
>                                     . str (snd state) . str ";" . nl

>                                   (LR'Fail) ->
>                                     str ""

>                                   (LR'MustFail) ->
>                                     str ""

>                             (first_terminal, last_terminal) =
>                                 bounds $ action ! 0

>                             goto_str nt_goto =
>                                 case nt_goto of
>                                   (nt, Goto next_state) ->
>                                       str "            when "
>                                     . str (snd $ nonterminals' !!
>                                            (nt - first_nonterm))
>                                     . str " =>" . nl
>                                     . str "              "
>                                     . str "s_push <= '1';" . nl
>                                     . str "              "
>                                     . str "s_push_data <= state"
>                                     . str (show next_state)
>                                     . str ";" . nl
>                                     . nl
>                                     . str "              "
>                                     . str "s_next_state <= state"
>                                     . str (show next_state)
>                                     . str ";" . nl . nl

>                                   (_, NoGoto) ->
>                                       str ""

>       next_state_update_process_str =
>           str "  next_state_update : process"
>         . str " ( s_current_state, s_current_nt," . nl
>         . str "                                "
>         . str "next_terminal_rdy, s_pop_data, terminal," . nl
>         . str "                                "
>         . str "s_current_reduce )" . nl
>         . str "  begin" . nl
>         . nl
>         . str "    next_terminal_ack <= '0';" . nl
>         . str "    parser_accept     <= '0';" . nl
>         . str "    parser_fail       <= '0';" . nl
>         . str "    s_push            <= '0';" . nl
>         . str "    s_push_data       <= fail;" . nl
>         . str "    s_pop             <= '0';" . nl
>         . str "    s_popn            <= 0;" . nl
>         . str "    s_next_reduce     <= '0';" . nl
>         . str "    s_next_nt         <= s_current_nt;" . nl
>         . str "    s_next_state      <= fail;" . nl
>         . nl
>         . states_case_str
>         . nl
>         . str "  end process;" . nl

>       current_state_update_process_str =
>           str "  current_state_update : process ( clock, reset )" . nl
>         . str "  begin" . nl
>         . str "    if reset = '1' then" . nl
>         . str "      s_current_nt     <= nonterminal_t'low;" . nl
>         . str "      s_current_state  <= start;" . nl
>         . str "      s_current_reduce <= '0';" . nl
>         . str "    elsif rising_edge (clock) then" . nl
>         . str "      s_current_nt     <= s_next_nt;" . nl
>         . str "      s_current_state  <= s_next_state;" . nl
>         . str "      s_current_reduce <= s_next_reduce;" . nl
>         . str "    end if;" . nl
>         . str "  end process;" . nl

>       entity_str =
>           str "entity parser is" . nl
>         . str "  port ( terminal          : in  terminal_t;" . nl
>         . str "         next_terminal_rdy : in  std_logic;" . nl
>         . str "         next_terminal_ack : out std_logic;" . nl
>         . str "         parser_accept," . nl
>         . str "         parser_fail       : out std_logic;" . nl
>         . str "         overflow_error," . nl
>         . str "         underflow_error   : out std_logic;" . nl
>         . str "         clock, reset      : in  std_logic" . nl
>         . str "  );" . nl
>         . str "end parser;" . nl

>       architecture_str =
>           str "architecture parser_architecture "
>         . str "of parser is" . nl
>         . nl
>         . str "  -- components" . nl
>         . str "  component stack is" . nl
>         . str "    port ( data_in         : in state_t;" . nl
>         . str "           data_out        : out state_t;" . nl
>         . str "           clock, reset    : in std_logic;" . nl
>         . str "           push, pop       : in std_logic;" . nl
>         . str "           popn            : in ram_index_t;" . nl
>         . str "           overflow_error," . nl
>         . str "           underflow_error : out std_logic" . nl
>         . str "    );" . nl
>         . str "  end component stack;" . nl
>         . nl
>         . str "  -- signals" . nl
>         . state_type_decl_str . nl
>         . str "  signal s_push_data, s_pop_data : state_t;" . nl
>         . str "  signal s_push, s_pop           : std_logic;" . nl
>         . str "  signal s_popn                  : ram_index_t;" . nl
>         . str "  signal s_current_reduce," . nl
>         . str "    s_next_reduce                : std_logic;" . nl
>         . str "  signal s_current_nt            : nonterminal_t;" . nl
>         . str "  signal s_next_nt               : nonterminal_t;" . nl
>         . nl
>         . str "begin" . nl
>         . nl
>         . str "  -- stack instantiation" . nl
>         . str "  stack_component : stack port map (" . nl
>         . str "    data_in         => s_push_data," . nl
>         . str "    data_out        => s_pop_data," . nl
>         . str "    clock           => clock," . nl
>         . str "    reset           => reset," . nl
>         . str "    push            => s_push," . nl
>         . str "    pop             => s_pop," . nl
>         . str "    popn            => s_popn," . nl
>         . str "    overflow_error  => overflow_error," . nl
>         . str "    underflow_error => underflow_error" . nl
>         . str "  );" . nl
>         . nl
>         . next_state_update_process_str . nl
>         . nl
>         . current_state_update_process_str . nl
>         . nl
>         . str "end parser_architecture;" . nl

> ieee_library_str =
>     str "library ieee;" . nl
>   . str "use ieee.std_logic_1164.all;" . nl

> stack_file_str base =
>    ieee_library_str
>  . str "use work." . str base . str "_types.all;" . nl
>  . nl
>  . str "entity stack is" . nl
>  . nl
>  . str "  port ( data_in         : in state_t;" . nl
>  . str "         data_out        : out state_t;" . nl
>  . str "         clock, reset    : in std_logic;" . nl
>  . str "         push, pop       : in std_logic;" . nl
>  . str "         popn            : in ram_index_t;" . nl
>  . str "         overflow_error," . nl
>  . str "         underflow_error : out std_logic" . nl
>  . str "  );" . nl
>  . nl
>  . str "end stack;" . nl
>  . nl
>  . str "architecture stack_architecture of stack is" . nl
>  . nl
>  . str "  signal s_ram             : ram_t := (others => state_t'low);" . nl
>  . str "  signal s_ram_index       : ram_index_t := ram_index_t'low;" . nl
>  . str "  signal s_write_enable    : std_logic;" . nl
>  . str "  signal s_data            : state_t;" . nl
>  . str "  signal s_overflow_error  : std_logic := '0';" . nl
>  . str "  signal s_underflow_error : std_logic := '0';" . nl
>  . nl
>  . str "  attribute ram_style          : string;" . nl
>  . str "  attribute ram_style of s_ram : signal is \"block\";" . nl
>  . nl
>  . str "begin" . nl
>  . nl
>  . str "  overflow_error <= s_overflow_error;" . nl
>  . str "  underflow_error <= s_underflow_error;" . nl
>  . nl
>  . str "  ram_process : process (clock, s_write_enable)" . nl
>  . str "  begin" . nl
>  . str "    if falling_edge (clock) then" . nl
>  . nl
>  . str "      if s_write_enable = '1' then" . nl
>  . str "        s_ram (s_ram_index) <= s_data;" . nl
>  . str "        data_out <= s_data;" . nl
>  . str "      else" . nl
>  . str "        data_out <= s_ram (s_ram_index);" . nl
>  . str "      end if;" . nl
>  . str "    end if;" . nl
>  . nl
>  . str "  end process;" . nl
>  . nl
>  . str "  push_pop_process : process (clock, reset, push)" . nl
>  . str "  begin" . nl
>  . nl
>  . str "    if reset = '1' then" . nl
>  . nl
>  . str "      s_ram_index <= ram_index_t'low;" . nl
>  . str "      s_overflow_error <= '0';" . nl
>  . str "      s_underflow_error <= '0';" . nl
>  . str "      s_write_enable <= '0';" . nl
>  . nl
>  . str "    else" . nl
>  . nl
>  . str "      if rising_edge (clock) then" . nl
>  . nl
>  . str "        s_overflow_error <= '0';" . nl
>  . str "        s_underflow_error <= '0';" . nl
>  . nl
>  . str "        s_write_enable <= '0';" . nl
>  . nl
>  . str "        if push = '1' then" . nl
>  . str "          if s_ram_index < ram_index_t'high then" . nl
>  . str "            s_ram_index <= s_ram_index + 1;" . nl
>  . str "            s_data <= data_in;" . nl
>  . str "            s_write_enable <= '1';" . nl
>  . str "          else" . nl
>  . str "            s_overflow_error <= '1';" . nl
>  . str "          end if;" . nl
>  . str "        elsif pop = '1' then" . nl
>  . str "          if s_ram_index >= ram_index_t'low + popn then" . nl
>  . str "            s_ram_index <= s_ram_index - popn;" . nl
>  . str "          else" . nl
>  . str "            s_ram_index <= ram_index_t'low;" . nl
>  . str "            s_underflow_error <= '1';" . nl
>  . str "          end if;" . nl
>  . str "        end if;" . nl
>  . nl
>  . str "      end if;" . nl
>  . nl
>  . str "    end if;" . nl
>  . nl
>  . str "  end process;" . nl
>  . nl
>  . str "end stack_architecture;" . nl

> types_file_str base states terminals nonterminals =
>    ieee_library_str
>  . nl

>  . str "package " . str base . str "_types is" . nl
>  . nl
>  . str "  constant ram_size : natural := 2**8;" . nl
>  . nl
>  . str "  subtype ram_index_t is natural range 0 to ram_size - 1;" . nl
>  . nl
>  . state_type_str
>  . nl
>  . terminal_type_str
>  . nl
>  . nonterminal_type_str
>  . nl
>  . str "  type ram_t is array ( ram_index_t ) of state_t;" . nl
>  . nl
>  . str "end " . str base . str "_types;" . nl
>  . nl
>      where state_type_str =
>                str ("  type state_t is ( "
>                     ++ (concat $
>                         intersperse
>                         ",\n                    "
>                         ("start" : "fail" : "update_reduction" :
>                          map snd states))
>                     ++ " );") . nl

>            terminal_type_str =
>                str ("  type terminal_t is ( "
>                     ++ (concat $
>                         intersperse
>                         ",\n                       "
>                         (map snd terminals))
>                     ++ " );") . nl

>            nonterminal_type_str =
>                str ("  type nonterminal_t is ( "
>                     ++ (concat $
>                         intersperse
>                         ",\n                          "
>                         (map snd nonterminals))
>                     ++ " );") . nl

> toplevel_file_str base states =
>     ieee_library_str
>   . nl
>   . str "use work." . str base . str "_types.all;" . nl
>   . str "use work.characters.all;" . nl
>   . nl

>   . str "entity toplevel is" . nl
>   . str "    port ( char               : in  character_t;" . nl
>   . str "           next_character_req : out std_logic;" . nl
>   . str "           lexer_fail         : out std_logic;" . nl
>   . str "           parser_accept," . nl
>   . str "           parser_fail        : out std_logic;" . nl
>   . str "           overflow_error," . nl
>   . str "           underflow_error    : out std_logic;" . nl
>   . str "           clock, reset       : in  std_logic" . nl
>   . str "    );" . nl
>   . str "end toplevel;" . nl
>   . nl

>   . str "architecture toplevel_architecture of toplevel is" . nl
>   . nl

>   . str "  -- components" . nl
>   . str "  component lexer is" . nl
>   . str "    port ( char               : in  character_t;" . nl
>   . str "           terminal           : out terminal_t;" . nl
>   . str "           next_character_req : out std_logic;" . nl
>   . str "           next_terminal_rdy  : out std_logic;" . nl
>   . str "           next_terminal_ack  : in  std_logic;" . nl
>   . str "           lexer_fail         : out std_logic;" . nl
>   . str "           clock, reset       : in  std_logic" . nl
>   . str "    );" . nl
>   . str "  end component lexer;" . nl
>   . nl
>   . nl
>   . str "  component parser is" . nl
>   . str "    port ( terminal          : in  terminal_t;" . nl
>   . str "           next_terminal_rdy : in  std_logic;" . nl
>   . str "           next_terminal_ack : out std_logic;" . nl
>   . str "           parser_accept," . nl
>   . str "           parser_fail       : out std_logic;" . nl
>   . str "           overflow_error," . nl
>   . str "           underflow_error   : out std_logic;" . nl
>   . str "           clock, reset      : in  std_logic" . nl
>   . str "    );" . nl
>   . str "  end component parser;" . nl
>   . nl
>   . nl
>   . str "  -- signals" . nl
>   . str "  signal s_terminal : terminal_t;" . nl
>   . str "  signal s_next_terminal_rdy  : std_logic;" . nl
>   . str "  signal s_next_terminal_ack  : std_logic;" . nl
>   . nl

>   . str "begin" . nl
>   . nl

>   . str "  -- lexer component instantiation" . nl
>   . str "  lexer_component : lexer port map (" . nl
>   . str "    char               => char," . nl
>   . str "    terminal           => s_terminal," . nl
>   . str "    next_character_req => next_character_req," . nl
>   . str "    next_terminal_rdy  => s_next_terminal_rdy," . nl
>   . str "    next_terminal_ack  => s_next_terminal_ack," . nl
>   . str "    lexer_fail         => lexer_fail," . nl
>   . str "    clock              => clock," . nl
>   . str "    reset              => reset" . nl
>   . str "  );" . nl
>   . nl
>   . nl
>   . str "  -- parser component instantiation" . nl
>   . str "  parser_component : parser port map (" . nl
>   . str "    terminal          => s_terminal," . nl
>   . str "    next_terminal_rdy => s_next_terminal_rdy," . nl
>   . str "    next_terminal_ack => s_next_terminal_ack," . nl
>   . str "    parser_accept     => parser_accept," . nl
>   . str "    parser_fail       => parser_fail," . nl
>   . str "    overflow_error    => overflow_error," . nl
>   . str "    underflow_error   => underflow_error," . nl
>   . str "    clock             => clock," . nl
>   . str "    reset             => reset" . nl
>   . str "  );" . nl

>   . nl
>   . nl
>   . str "end toplevel_architecture;" . nl
