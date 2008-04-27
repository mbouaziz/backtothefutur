using System;
using Electronics;

namespace Intel400x
{
    public enum Cycle4004
    {
        A1, A2, A3, M1, M2, X1, X2, X3
    }
    public enum IOOperations
    {
        NOP, WRM, WMP, WRR,
        WR0, WR1, WR2, WR3,
        S8M, RDM, RDR, ADM,
        RD0, RD1, RD2, RD3
    }

    public enum Pins4004
    {
        D0, D1, D2, D3,
        VSS, Clk1, Clk2, Sync,
        Reset, Test, CMROM, VDD,
        CMRAM3, CMRAM2, CMRAM1, CMRAM0
    }

    public class Intel4004 : AsyncComponent<Pins4004>
    {
        PinsGroup D, CMRAM;

        Intel4004(string name, Signal[] signals)
            : base(name, signals)
        {
            Pins[(int)Pins4001.Clk1].ondown += onclk1down;
            Pins[(int)Pins4001.Clk2].ondown += onclk2down;
            Pins[(int)Pins4001.Clk2].onup += onclk2up;
            Pins[(int)Pins4001.Reset].ondown += onresetdown;
            D = makePinsGroup("D", (int)Pins4004.D0, (int)Pins4004.D3);
            CMRAM = makePinsGroup("CMRAM", (int)Pins4004.CMRAM0, (int)Pins4004.CMRAM3);
        }
        public override string FamilyName
        {
            get { return "4004"; }
        }

        Cycle4004 cycle;

        void onclk1down()
        {
            
        }
        void onclk2down()
        {

        }
        void onclk2up()
        {
            if (cycle == Cycle4004.X3)
            {
                Pins[(int)Pins4004.Sync].latch(false);
                cycle = Cycle4004.A1;
            }
            else
            {
                Pins[(int)Pins4004.Sync].latch(true);
                cycle++;
            }
        }
        void onresetdown()
        {
        }
    }
}
