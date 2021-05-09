
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

entity TestBench is

end TestBench;

architecture Behavioral of TestBench is
    SIGNAL CLK: std_logic;
    SIGNAL REG_A: std_logic_vector(7 downto 0); 
    SIGNAL REG_B : std_logic_vector(7 downto 0);
    SIGNAL REG_C : std_logic_vector(7 downto 0);
    SIGNAL REG_D : std_logic_vector(7 downto 0);
    SIGNAL REG_E : std_logic_vector(7 downto 0);
    SIGNAL REG_H : std_logic_vector(7 downto 0);
    SIGNAL REG_L :  std_logic_vector(7 downto 0);
    SIGNAL IR : std_logic_vector(8 downto 0);
    SIGNAL PC: std_logic_vector(8 downto 0);
    signal Z: std_logic;    
    
    
component Proc9090
PORT(
        CLK: in std_logic;
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
end component;

begin
    
TestProc : Proc9090
    PORT MAP(
        CLK=>CLK,
        PROC_REG_A => REG_A,
        PROC_REG_B => REG_B,
        PROC_REG_C => REG_C,
        PROC_REG_D => REG_D,
        PROC_REG_E => REG_E,
        PROC_REG_H => REG_H,
        PROC_REG_L => REG_L,
        PROC_IR => IR,
        PROC_PC => PC,
        PROC_Z => Z
        );
        
process 
    begin
    for Z in 1 to 9999 loop
        clk <='1';
        wait for 10 ns;
        clk <= '0';
        wait for 10 ns;
        end loop;
end process;

end Behavioral;