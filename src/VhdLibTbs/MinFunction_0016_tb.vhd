----------------------------------------------------------------------------------------------------
--                                   Testbench for MinFunction
----------------------------------------------------------------------------------------------------
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use work.VhdSimToolsPkg.all;
use work.MinFunction_0016;

entity MinFunction_0016_tb is
end MinFunction_0016_tb;


architecture behavioral_MinFunction_0016_tb of MinFunction_0016_tb is


component MinFunction_0016 is 
    generic ( 
        max_integer : integer := (2**30) - 2
    );    
    port (
        clk : in std_logic;
        rst : in std_logic;
        i_soft_rst : in std_logic;
        i_distance_0000 : in integer := max_integer;
        i_distance_0001 : in integer := max_integer;
        i_distance_0002 : in integer := max_integer;
        i_distance_0003 : in integer := max_integer;
        i_distance_0004 : in integer := max_integer;
        i_distance_0005 : in integer := max_integer;
        i_distance_0006 : in integer := max_integer;
        i_distance_0007 : in integer := max_integer;
        i_distance_0008 : in integer := max_integer;
        i_distance_0009 : in integer := max_integer;
        i_distance_0010 : in integer := max_integer;
        i_distance_0011 : in integer := max_integer;
        i_distance_0012 : in integer := max_integer;
        i_distance_0013 : in integer := max_integer;
        i_distance_0014 : in integer := max_integer;
        i_distance_0015 : in integer := max_integer;
        o_min : out integer;
        o_latency : out integer
    ); 
end component;


signal clk : std_logic := '1';
signal rst : std_logic := '1';
signal i_soft_rst : std_logic;
signal i_distance_0000 : integer;
signal i_distance_0001 : integer;
signal i_distance_0002 : integer;
signal i_distance_0003 : integer;
signal i_distance_0004 : integer;
signal i_distance_0005 : integer;
signal i_distance_0006 : integer;
signal i_distance_0007 : integer;
signal i_distance_0008 : integer;
signal i_distance_0009 : integer;
signal i_distance_0010 : integer;
signal i_distance_0011 : integer;
signal i_distance_0012 : integer;
signal i_distance_0013 : integer;
signal i_distance_0014 : integer;
signal i_distance_0015 : integer;
signal o_min : integer;
signal o_latency : integer;

signal correct_answer : integer;

constant clk_per : time := 10 ns;
signal sim_done : std_logic := '0';
signal test_stage : integer := 0;


begin


----------------------------------------------------------------------------------------------------
--                                          Boiler Plate
----------------------------------------------------------------------------------------------------
CLOCK_PROCESS: process
begin
    if sim_done = '1' then
        wait;
    else
        wait for clk_per/2;
        clk <= not clk;
    end if;
end process;


----------------------------------------------------------------------------------------------------
--                                          Stim Process
----------------------------------------------------------------------------------------------------
STIM_PROCESS: process
begin
    i_soft_rst <= '0';
    i_distance_0000 <= 400;
    i_distance_0001 <= 900;
    i_distance_0002 <= 3076;
    i_distance_0003 <= 200;
    i_distance_0004 <= 201;
    i_distance_0005 <= 202;
    i_distance_0006 <= 409;
    i_distance_0007 <= 8800;
    i_distance_0008 <= 1000;
    i_distance_0009 <= 117;
    i_distance_0010 <= 2010;
    i_distance_0011 <= 2011;
    i_distance_0012 <= 2012;
    i_distance_0013 <= 2013;
    i_distance_0014 <= 2014;
    i_distance_0015 <= 2015;
    correct_answer <= 117;
    sync_wait_rising(clk, 10);
    rst <= not rst;
    sync_wait_rising(clk, 5);
    
    i_distance_0000 <= 878243;
    i_distance_0001 <= 865315;
    i_distance_0002 <= 759749;
    i_distance_0003 <= 515622;
    i_distance_0004 <= 148355;
    i_distance_0005 <= 310186;
    i_distance_0006 <= 754128;
    i_distance_0007 <= 997361;
    i_distance_0008 <= 270769;
    i_distance_0009 <= 917680;
    i_distance_0010 <= 920465;
    i_distance_0011 <= 20147;
    i_distance_0012 <= 494802;
    i_distance_0013 <= 265296;
    i_distance_0014 <= 754357;
    i_distance_0015 <= 538014;
    correct_answer <= 20147;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 87843;
    i_distance_0001 <= 718951;
    i_distance_0002 <= 451528;
    i_distance_0003 <= 379113;
    i_distance_0004 <= 1018889;
    i_distance_0005 <= 440489;
    i_distance_0006 <= 508845;
    i_distance_0007 <= 508237;
    i_distance_0008 <= 549520;
    i_distance_0009 <= 716081;
    i_distance_0010 <= 106644;
    i_distance_0011 <= 840181;
    i_distance_0012 <= 948022;
    i_distance_0013 <= 888532;
    i_distance_0014 <= 303770;
    i_distance_0015 <= 639324;
    correct_answer <= 87843;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 35776;
    i_distance_0001 <= 908831;
    i_distance_0002 <= 788205;
    i_distance_0003 <= 89645;
    i_distance_0004 <= 675407;
    i_distance_0005 <= 530991;
    i_distance_0006 <= 847796;
    i_distance_0007 <= 287381;
    i_distance_0008 <= 105174;
    i_distance_0009 <= 378071;
    i_distance_0010 <= 847192;
    i_distance_0011 <= 178741;
    i_distance_0012 <= 623708;
    i_distance_0013 <= 876445;
    i_distance_0014 <= 438653;
    i_distance_0015 <= 148671;
    correct_answer <= 35776;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 116352;
    i_distance_0001 <= 597921;
    i_distance_0002 <= 938754;
    i_distance_0003 <= 208387;
    i_distance_0004 <= 950637;
    i_distance_0005 <= 792431;
    i_distance_0006 <= 574041;
    i_distance_0007 <= 695921;
    i_distance_0008 <= 548817;
    i_distance_0009 <= 221044;
    i_distance_0010 <= 477974;
    i_distance_0011 <= 247065;
    i_distance_0012 <= 623098;
    i_distance_0013 <= 960059;
    i_distance_0014 <= 351452;
    i_distance_0015 <= 414462;
    correct_answer <= 116352;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 885026;
    i_distance_0001 <= 46693;
    i_distance_0002 <= 331302;
    i_distance_0003 <= 316325;
    i_distance_0004 <= 837766;
    i_distance_0005 <= 284201;
    i_distance_0006 <= 328266;
    i_distance_0007 <= 517869;
    i_distance_0008 <= 474172;
    i_distance_0009 <= 595439;
    i_distance_0010 <= 365680;
    i_distance_0011 <= 81587;
    i_distance_0012 <= 662678;
    i_distance_0013 <= 437434;
    i_distance_0014 <= 577244;
    i_distance_0015 <= 688095;
    correct_answer <= 46693;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 511264;
    i_distance_0001 <= 185889;
    i_distance_0002 <= 152225;
    i_distance_0003 <= 927178;
    i_distance_0004 <= 864107;
    i_distance_0005 <= 555820;
    i_distance_0006 <= 694638;
    i_distance_0007 <= 507375;
    i_distance_0008 <= 48718;
    i_distance_0009 <= 684465;
    i_distance_0010 <= 537458;
    i_distance_0011 <= 1029171;
    i_distance_0012 <= 838964;
    i_distance_0013 <= 838453;
    i_distance_0014 <= 489175;
    i_distance_0015 <= 481305;
    correct_answer <= 48718;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 151781;
    i_distance_0001 <= 124934;
    i_distance_0002 <= 149066;
    i_distance_0003 <= 960299;
    i_distance_0004 <= 34412;
    i_distance_0005 <= 768493;
    i_distance_0006 <= 664811;
    i_distance_0007 <= 540975;
    i_distance_0008 <= 960497;
    i_distance_0009 <= 226194;
    i_distance_0010 <= 214323;
    i_distance_0011 <= 344371;
    i_distance_0012 <= 486933;
    i_distance_0013 <= 62743;
    i_distance_0014 <= 738395;
    i_distance_0015 <= 568540;
    correct_answer <= 34412;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 357632;
    i_distance_0001 <= 605955;
    i_distance_0002 <= 978883;
    i_distance_0003 <= 479397;
    i_distance_0004 <= 498436;
    i_distance_0005 <= 803879;
    i_distance_0006 <= 89069;
    i_distance_0007 <= 376431;
    i_distance_0008 <= 88467;
    i_distance_0009 <= 742707;
    i_distance_0010 <= 623061;
    i_distance_0011 <= 511478;
    i_distance_0012 <= 966648;
    i_distance_0013 <= 196026;
    i_distance_0014 <= 308604;
    i_distance_0015 <= 287199;
    correct_answer <= 88467;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 960066;
    i_distance_0001 <= 735879;
    i_distance_0002 <= 746217;
    i_distance_0003 <= 705035;
    i_distance_0004 <= 723409;
    i_distance_0005 <= 58417;
    i_distance_0006 <= 726739;
    i_distance_0007 <= 145108;
    i_distance_0008 <= 627639;
    i_distance_0009 <= 78710;
    i_distance_0010 <= 886583;
    i_distance_0011 <= 7576;
    i_distance_0012 <= 911730;
    i_distance_0013 <= 991418;
    i_distance_0014 <= 843325;
    i_distance_0015 <= 585151;
    correct_answer <= 7576;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 406754;
    i_distance_0001 <= 183907;
    i_distance_0002 <= 326280;
    i_distance_0003 <= 136894;
    i_distance_0004 <= 681102;
    i_distance_0005 <= 866095;
    i_distance_0006 <= 88370;
    i_distance_0007 <= 400403;
    i_distance_0008 <= 168307;
    i_distance_0009 <= 551743;
    i_distance_0010 <= 72147;
    i_distance_0011 <= 343895;
    i_distance_0012 <= 303669;
    i_distance_0013 <= 527964;
    i_distance_0014 <= 685310;
    i_distance_0015 <= 795743;
    correct_answer <= 72147;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 369952;
    i_distance_0001 <= 361793;
    i_distance_0002 <= 46754;
    i_distance_0003 <= 560483;
    i_distance_0004 <= 97051;
    i_distance_0005 <= 583968;
    i_distance_0006 <= 569604;
    i_distance_0007 <= 18633;
    i_distance_0008 <= 741675;
    i_distance_0009 <= 949036;
    i_distance_0010 <= 184110;
    i_distance_0011 <= 511217;
    i_distance_0012 <= 375346;
    i_distance_0013 <= 617144;
    i_distance_0014 <= 98107;
    i_distance_0015 <= 425565;
    correct_answer <= 18633;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 172544;
    i_distance_0001 <= 337218;
    i_distance_0002 <= 719268;
    i_distance_0003 <= 28484;
    i_distance_0004 <= 588874;
    i_distance_0005 <= 295819;
    i_distance_0006 <= 893997;
    i_distance_0007 <= 201329;
    i_distance_0008 <= 459218;
    i_distance_0009 <= 1007379;
    i_distance_0010 <= 102229;
    i_distance_0011 <= 674265;
    i_distance_0012 <= 694394;
    i_distance_0013 <= 177853;
    i_distance_0014 <= 481278;
    i_distance_0015 <= 372735;
    correct_answer <= 28484;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 642624;
    i_distance_0001 <= 590050;
    i_distance_0002 <= 724835;
    i_distance_0003 <= 286980;
    i_distance_0004 <= 727654;
    i_distance_0005 <= 331145;
    i_distance_0006 <= 344607;
    i_distance_0007 <= 711150;
    i_distance_0008 <= 181970;
    i_distance_0009 <= 910133;
    i_distance_0010 <= 1036118;
    i_distance_0011 <= 415446;
    i_distance_0012 <= 833944;
    i_distance_0013 <= 98620;
    i_distance_0014 <= 905182;
    i_distance_0015 <= 662015;
    correct_answer <= 98620;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 273824;
    i_distance_0001 <= 138241;
    i_distance_0002 <= 610630;
    i_distance_0003 <= 294303;
    i_distance_0004 <= 715308;
    i_distance_0005 <= 725391;
    i_distance_0006 <= 440721;
    i_distance_0007 <= 947281;
    i_distance_0008 <= 470485;
    i_distance_0009 <= 298422;
    i_distance_0010 <= 970071;
    i_distance_0011 <= 972214;
    i_distance_0012 <= 715711;
    i_distance_0013 <= 405691;
    i_distance_0014 <= 84924;
    i_distance_0015 <= 813279;
    correct_answer <= 84924;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 347424;
    i_distance_0001 <= 196227;
    i_distance_0002 <= 246244;
    i_distance_0003 <= 106847;
    i_distance_0004 <= 582568;
    i_distance_0005 <= 23624;
    i_distance_0006 <= 841195;
    i_distance_0007 <= 906187;
    i_distance_0008 <= 990353;
    i_distance_0009 <= 196466;
    i_distance_0010 <= 537585;
    i_distance_0011 <= 317078;
    i_distance_0012 <= 38520;
    i_distance_0013 <= 346809;
    i_distance_0014 <= 908573;
    i_distance_0015 <= 698623;
    correct_answer <= 23624;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 730880;
    i_distance_0001 <= 545729;
    i_distance_0002 <= 628959;
    i_distance_0003 <= 635135;
    i_distance_0004 <= 200484;
    i_distance_0005 <= 18277;
    i_distance_0006 <= 911301;
    i_distance_0007 <= 720930;
    i_distance_0008 <= 254441;
    i_distance_0009 <= 395787;
    i_distance_0010 <= 359469;
    i_distance_0011 <= 328752;
    i_distance_0012 <= 763734;
    i_distance_0013 <= 689240;
    i_distance_0014 <= 341629;
    i_distance_0015 <= 1025055;
    correct_answer <= 18277;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 699964;
    i_distance_0001 <= 218114;
    i_distance_0002 <= 354371;
    i_distance_0003 <= 312227;
    i_distance_0004 <= 397478;
    i_distance_0005 <= 683433;
    i_distance_0006 <= 291275;
    i_distance_0007 <= 732847;
    i_distance_0008 <= 593811;
    i_distance_0009 <= 867508;
    i_distance_0010 <= 325171;
    i_distance_0011 <= 367509;
    i_distance_0012 <= 365747;
    i_distance_0013 <= 343640;
    i_distance_0014 <= 98108;
    i_distance_0015 <= 845758;
    correct_answer <= 98108;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 871488;
    i_distance_0001 <= 779681;
    i_distance_0002 <= 830242;
    i_distance_0003 <= 154560;
    i_distance_0004 <= 910340;
    i_distance_0005 <= 580840;
    i_distance_0006 <= 380425;
    i_distance_0007 <= 302160;
    i_distance_0008 <= 302897;
    i_distance_0009 <= 762417;
    i_distance_0010 <= 437750;
    i_distance_0011 <= 874294;
    i_distance_0012 <= 135702;
    i_distance_0013 <= 605818;
    i_distance_0014 <= 31261;
    i_distance_0015 <= 311295;
    correct_answer <= 31261;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 672005;
    i_distance_0001 <= 388038;
    i_distance_0002 <= 624774;
    i_distance_0003 <= 541193;
    i_distance_0004 <= 699562;
    i_distance_0005 <= 470731;
    i_distance_0006 <= 149325;
    i_distance_0007 <= 311662;
    i_distance_0008 <= 895058;
    i_distance_0009 <= 894771;
    i_distance_0010 <= 19059;
    i_distance_0011 <= 648757;
    i_distance_0012 <= 629524;
    i_distance_0013 <= 64630;
    i_distance_0014 <= 129752;
    i_distance_0015 <= 814363;
    correct_answer <= 19059;
    sync_wait_rising(clk, 10);

    i_distance_0000 <= 210113;
    i_distance_0001 <= 245282;
    i_distance_0002 <= 476931;
    i_distance_0003 <= 942724;
    i_distance_0004 <= 142408;
    i_distance_0005 <= 344991;
    i_distance_0006 <= 174591;
    i_distance_0007 <= 814989;
    i_distance_0008 <= 500816;
    i_distance_0009 <= 652242;
    i_distance_0010 <= 173778;
    i_distance_0011 <= 657076;
    i_distance_0012 <= 491445;
    i_distance_0013 <= 400600;
    i_distance_0014 <= 220923;
    i_distance_0015 <= 598751;
    correct_answer <= 142408;
    sync_wait_rising(clk, 10);

    sync_wait_rising(clk, 100);
    sim_done <= '1';
    wait;
end process;




----------------------------------------------------------------------------------------------------
--                                        Unit Under Test
----------------------------------------------------------------------------------------------------
UUT: MinFunction_0016 port map (
    clk => clk,
    rst => rst,
    i_soft_rst => i_soft_rst,
    i_distance_0000 => i_distance_0000,
    i_distance_0001 => i_distance_0001,
    i_distance_0002 => i_distance_0002,
    i_distance_0003 => i_distance_0003,
    i_distance_0004 => i_distance_0004,
    i_distance_0005 => i_distance_0005,
    i_distance_0006 => i_distance_0006,
    i_distance_0007 => i_distance_0007,
    i_distance_0008 => i_distance_0008,
    i_distance_0009 => i_distance_0009,
    i_distance_0010 => i_distance_0010,
    i_distance_0011 => i_distance_0011,
    i_distance_0012 => i_distance_0012,
    i_distance_0013 => i_distance_0013,
    i_distance_0014 => i_distance_0014,
    i_distance_0015 => i_distance_0015,
    o_min => o_min,
    o_latency => o_latency
);


end architecture behavioral_MinFunction_0016_tb;


