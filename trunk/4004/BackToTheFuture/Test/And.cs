using System;
using System.Collections.Generic;
using System.Text;
using Electronics;

namespace Test
{
    public enum AndPinsIn
    {
        In0, In1
    }
    public enum AndPinsOut
    {
        Out
    }

    /*
    public class And : NetComponentClient<AndPinsIn, AndPinsOut>
    {
        public And(string name, Signal[] signalsIn, Signal[] signalsOut)
              : base(name, signalsIn, signalsOut, "felouque", 1400)
        {
        }

        public override string FamilyName
        {
            get { return "And"; }
        }
    }
    */

    public class And : Component<AndPinsIn, AndPinsOut>
    {
        public And(string name, Signal[] sIn, Signal[] sOut)
            : base(name, sIn, sOut)
        {
        }

        public override void Clk()
        {
            PinsOut[(int)AndPinsOut.Out].latch(PinsIn[(int)AndPinsIn.In0].value && PinsIn[(int)AndPinsIn.In1].value);
        }

        public override string FamilyName
        {
            get { return "And"; }
        }
    }
}
