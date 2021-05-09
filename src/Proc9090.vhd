----------------------------------------------------------------------------------
-- Proc9090 Simulation Processor
-- ISA:
-- MULT     000     dst*src and store in HL
-- MOV      001     dst src
-- ADD      010     dst src
-- BRA      011     branch to absolute location
-- DIV      100     divide HL by src and store result in dst.. set Z flag
-- MOD      101     divide HL by src and store remainder in dst.. set Z flag
-- BEZ      110     branch to absolute location
-- SUB      111     subtract dst and src and set Z, discard result

-- REGISTERS:
-- 8 8-bit registers indexed as 000-B, 001-C, 010-D, 011-E, 100-H, 101-L, 110-mem, 111-A

-- PROGRAM TO GENERATE PRIMES IN REG A (0x02 loaded into HL already).
-- REGS HL=i, B=j, D=remainder A=primes, C=2, E=1, mem=255
-- 0 outer:  MOV B C -- load 2 into B (j)
-- 1         ADD L, E -- increment i
-- 2 loop:   MOD D, B -- i%j -> D
-- 3         BEZ outer
-- 4         ADD B, E -- increment j
-- 5         SUB L, B -- L - B to test if B==L (we're done)_
-- 6         BEZ prime
-- 7         BRA loop
-- 8 prime:   MOV A, L -- load the prime into A
-- 9         BRA outer
-- "001000001", "010101011", "101010000", 
-- "110000000", "010000011", "111101000",
-- "110001000", "011000010", "001111101",
-- "011000000"
            
----------------------------------------------------------------------------------

library IEEE; use IEEE.STD_LOGIC_1164.ALL; use IEEE.NUMERIC_STD.ALL;

-- Define Divider and Multiplier units
entity Divider is
    generic(N : integer := 8);
    Port (  P: in std_logic_vector(2*N-1 downto 0);
            DIV_BY: in std_logic_vector(N-1 downto 0);
            RESULT: out std_logic_vector(N-1 downto 0);
            REMAINDER: out std_logic_vector(N-1 downto 0);
            CLK, START: in std_logic;        
            DONE: out std_logic                   
          );
end entity Divider;
library IEEE; use IEEE.STD_LOGIC_1164.ALL; use IEEE.NUMERIC_STD.ALL;
entity Multiplier is
    generic(N : integer := 8);
    Port (  A, B: in std_logic_vector(N-1 downto 0);
            P: inout std_logic_vector(2*N-1 downto 0); 
            CLK, START: in std_logic;         
            DONE: out std_logic           
          );
end entity Multiplier;

architecture BehavioralDiv of Divider is
    type states is (STOP, INIT, SUBTRACTING);
	signal state: states := STOP;
	signal P_IN: unsigned(2*N-1 downto 0);
	signal DIV_BY_IN: unsigned(N-1 downto 0);
	signal tmp: unsigned(N-1 downto 0);
begin
    process(CLK)
	begin
		if rising_edge(CLK) then
			case state is
				when STOP =>
				    -- Move to the next state if we get a start signal.
				    DONE <= '0';
				    tmp <= (others => '0');
					if START = '1' then
                        -- Start working and clock in our input
						state <= INIT;
                        P_IN <= unsigned(P);
    					DIV_BY_IN <= unsigned(DIV_BY);
					end if;
				when INIT =>
				    -- Initialize internal signals for the divide algo.
				    DONE <= '0';
					RESULT <= (others => '0');
					tmp <= (others => '0');
					state <= SUBTRACTING;
				when SUBTRACTING => 
				    -- As long as we arent dividing by 0, each cycle subtract the dividend by
				    -- the divisor and increment the temp result by 1.
				    -- If we cant do this anymore, the temp_result is the result and we're done.
				    if (P_IN >= DIV_BY_IN and DIV_BY_IN /= 0) then
				        P_IN <= P_IN - DIV_BY_IN;
				        tmp <= tmp + 1;
				        DONE <= '0';
				    else 
				        -- The division is done because we can't subtract anymore;
				        RESULT <= std_logic_vector(tmp);
				        REMAINDER <= std_logic_vector(P_IN(7 downto 0));
				        DONE <= '1';
				        state <= STOP;
				    end if;
			end case;
		end if;
	end process;
end BehavioralDiv;

architecture BehavioralMult of Multiplier is
    type states is (STOP, INIT, ADDING, SHIFTING);
	signal state: states := STOP;
	signal cnt: unsigned(N-1 downto 0);
	signal carry: std_logic := '0';
	signal sum : std_logic_vector(N downto 0) := (others=>'0');
begin
    process(CLK)
	begin
		if rising_edge(CLK) then
			case state is
				when STOP =>
				    DONE <= '0';
				    P <= (others => '0');
					if start = '1' then
						state <= INIT;
					end if;
				when INIT =>
				    DONE <= '0';
					state <= ADDING;
					P <= (2*N-1 downto B'length => '0') & B;
					cnt <= to_unsigned(0,N);
				when ADDING => 
						state <= SHIFTING;
						DONE <= '0';
						if (P(0) = '1') then 
						  P <= std_logic_vector(unsigned(P(2*N-1 downto N)) + unsigned(A) ) & P(N-1 downto 0);
						else 
						  carry <= '0';
						end if;
				when SHIFTING => 
        				cnt <= cnt + 1;
    					P <= carry & P(2*N-1 downto 1); -- do the right shift
						if(CNT = N-1) then
							state <= STOP;
							DONE <= '1';
						else
							state <= ADDING;
						end if;
			end case;
		end if;
	end process;
end BehavioralMult;


-- TOP-LEVEL PROCESSOR
library IEEE; use IEEE.STD_LOGIC_1164.ALL; use IEEE.NUMERIC_STD.ALL;
ENTITY Proc9090 IS 
    Port(
        CLK: in std_logic;
        
        -- PROC REGISTERS AND FLAGS
        PROC_REG_A: out std_logic_vector (7 downto 0);
        PROC_REG_B: out std_logic_vector (7 downto 0);
        PROC_REG_C: out std_logic_vector (7 downto 0);
        PROC_REG_D: out std_logic_vector (7 downto 0);
        PROC_REG_E: out std_logic_vector (7 downto 0);
        PROC_REG_H: out std_logic_vector (7 downto 0);
        PROC_REG_L: out std_logic_vector (7 downto 0);
        PROC_IR: out std_logic_vector (8 downto 0);
        PROC_PC: out std_logic_vector (8 downto 0);
        PROC_Z: out std_logic
    );
END Proc9090;

ARCHITECTURE BehaviourProc9090 OF Proc9090 IS 
    component Multiplier is
		port (A, B: in std_logic_vector(7 downto 0);
		      P: inout std_logic_vector(15 downto 0);
		      CLK, START: in std_logic;
		      DONE: out std_logic);
	end component;
	
	component Divider is 
	   port (
	        P: in std_logic_vector(15 downto 0);
            DIV_BY: in std_logic_vector(7 downto 0);
            RESULT: out std_logic_vector(7 downto 0);
            REMAINDER: out std_logic_vector(7 downto 0);
            CLK, START: in std_logic; 
            DONE: out std_logic);
	end component;
    
    type myArray is array (0 to 9) of unsigned(8 downto 0); 
    signal Mem:myArray:=("001000001", "010101011", "101010000", 
                             "110000000", "010000011", "111101000",
                             "110001000", "011000010", "001111101",
                             "011000000");
    type myRegs is array (0 to 7) of unsigned(7 downto 0); 
    
    -- NOTE: From 8080; registers are indexed as 000-B, 001-C, D, E, H, L, mem, A
    -- For this prime num algo we load REGS HL=i, B=j, D=remainder A=primes, C=2, E=1, mem=255
    signal Reg:myRegs:=(x"02",x"02",x"DD",x"01",x"00",x"01",x"FF",x"02");
    signal RSTc : std_logic := '1';
    signal Tc: unsigned(7 downto 0) := x"00"; 
    signal PC,IR: unsigned(8 downto 0) := "000000000"; 
    signal Z: std_logic := '0';
    
    signal start_multiply: std_logic := '0';
    signal done_multiply: std_logic := '0';
    signal multiply_product : std_logic_vector (15 downto 0);
    type mult_states is (INIT, MULTIPLYING);
	signal mult_state: mult_states := INIT;
	
	type div_states is (INIT, DIVIDING); 
	signal div_state: div_states := INIT;
	signal start_divide: std_logic := '0';
	signal done_divide: std_logic := '0';
	signal div_by: std_logic_vector(7 downto 0);
	signal div_result: std_logic_vector(7 downto 0);
	signal div_remainder: std_logic_vector(7 downto 0);
	signal prod_div: std_logic_vector(15 downto 0);
	
	
	type sub_states is (FIRST, SECOND);
	signal sub_state: sub_states := FIRST;
    signal sub_res: unsigned(7 downto 0) := "00000000";
	signal sub_res_tmp: unsigned(7 downto 0) := (others => '0');
    begin 
        multiplier_unit: Multiplier port map (
                    A => std_logic_vector(Reg(7)), 
                    B => std_logic_vector(Reg(0)), 
                    P => multiply_product, 
                    CLK => clk, 
                    START=>start_multiply,
                    DONE=>done_multiply
                    );
        divider_unit: Divider port map (
            P => prod_div,
            DIV_BY => div_by,
            RESULT => div_result,
            REMAINDER => div_remainder,
            CLK => clk,
            START => start_divide,
            DONE => done_divide
        );
      
        process(clk)
        variable src,dst: integer;
        begin
         if (clk'event and clk='1') then
           src := to_integer(IR(2 downto 0));
           dst := to_integer(IR(5 downto 3));
           sub_res_tmp <= Reg(dst)-Reg(src);

            -- Update our testbench signals
            PROC_REG_A <= std_logic_vector(Reg(7));
            PROC_REG_B <= std_logic_vector(Reg(0));
            PROC_REG_C <= std_logic_vector(Reg(1));
            PROC_REG_D <= std_logic_vector(Reg(2));
            PROC_REG_E <= std_logic_vector(Reg(3));
            PROC_REG_H <= std_logic_vector(Reg(4));
            PROC_REG_L <= std_logic_vector(Reg(5));
            PROC_IR <= std_logic_vector(IR);
            PROC_PC <= std_logic_vector(PC);
            PROC_Z <= Z;

                     
           CASE Tc is
             when x"00" => 
                IR <= Mem(to_integer(PC)); 
                RSTc <= '0';
                Tc <= Tc+1;
             when x"01" => 
                CASE IR(8 downto 6) is
                  when "001" =>  
                    -- MOV
                    Reg(dst)<=Reg(src);
                    Tc<=Tc+1;
                  when "010" =>
                     -- ADD
                     Reg(dst) <= Reg(dst)+Reg(src);
                     Tc<=Tc+1;
                  when "011" => 
                     -- BRA
                     PC <= "000" & IR(5 downto 0);
                     RSTc<='1';
                     Tc <= Tc+1;
                  when "000" =>
                    -- MULT
                    case mult_state is
                        when INIT =>
                            start_multiply <= '1';
                            mult_state <= MULTIPLYING;
                        when MULTIPLYING =>
                            if (done_multiply = '1') then 
                                start_multiply <= '0';
                                Reg(4) <= unsigned( multiply_product(15 downto 8) );
                                Reg(5) <= unsigned( multiply_product(7 downto 0) );
                                mult_state <= INIT;
                                Tc <= Tc+1; -- Move onto next state
                            end if;
                    end case;
                  when "100" =>
                    -- DIV
                    prod_div <= std_logic_vector(Reg(4)) & std_logic_vector(Reg(5));
                    div_by <= std_logic_vector(Reg(src));
                    case div_state is
                        when INIT =>
                            div_by <= std_logic_vector(Reg(src));
                            start_divide <= '1';
                            div_state <= DIVIDING;
                        when DIVIDING =>
                            start_divide <= '0';
                            if (done_divide = '1') then 
                                Reg(dst) <= unsigned(div_result);
                                if (unsigned(div_result) = "0") then Z <= '1'; end if; if (unsigned(div_result) = "1") then Z <= '0'; end if; 
                                div_state <= INIT;
                                Tc <= Tc+1;
                            end if;
                    end case;
                  when "101" =>
                    -- MOD 
                    prod_div <= std_logic_vector(Reg(4)) & std_logic_vector(Reg(5));
                    div_by <= std_logic_vector(Reg(src));
                    case div_state is
                        when INIT =>
                            start_divide <= '1';
                            div_state <= DIVIDING;
                        when DIVIDING =>
                            start_divide <= '0';
                            if (done_divide = '1') then 
                                Reg(dst) <= unsigned(div_remainder);
                                div_state <= INIT;
                                Tc <= Tc+1;    
                                if (unsigned(div_remainder) = "0") then Z <= '1'; end if; if (unsigned(div_remainder) = "1") then Z <= '0'; end if; 
                            end if;
                    end case;
                  when "110" =>
                    -- BEZ
                    if(Z='1') then 
                        PC <= "000" & IR(5 downto 0);
                    else 
                        PC <= PC + 1;
                    end if;
                     RSTc<='1';
                     Tc <= Tc+1;
                  when "111" =>
                    -- SUB  dst and src and set Z
                    case sub_state is
                        when FIRST =>
                            if sub_res_tmp = x"00" then Z <= '1'; else Z <= '0'; end if;
                            sub_state <= SECOND;
                        when SECOND =>
                            if sub_res_tmp = x"00" then Z <= '1'; else Z <= '0'; end if;
                            RSTc<='1';
                            Tc <= Tc+1;
                            sub_state <= FIRST;
                    end case;
                 
                  when others => 
                    if (RSTc='1') then Tc<=x"00"; else Tc<=Tc+1; end if;
                  end case;
             when x"02" => 
                if (IR(8 downto 6) /= "110" and IR(8 downto 6) /= "011") then
                    -- If not BRA or BEZ
                    PC<=PC+1;  
                end if;
                RSTc<='1';
                Tc <= x"00";
             when others => 
                if (RSTc='1') then Tc<=x"00"; else Tc<=Tc+1; end if;
             end case;
        end if; 
        end process; 
    END BehaviourProc9090;


