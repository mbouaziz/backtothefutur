using System;

namespace Intel400x
{
    public abstract class Component<PinsEnumT>
    {
        private string _name;
        protected Pin[] pins;

        public Component(string name, Signal[] signals)
        {
            this._name = name;

            int nb_pins = Enum.GetValues(typeof(PinsEnumT)).Length;

            if (signals.Length != nb_pins)
                throw new Exception("Invalid signals' number");

            pins = new Pin[nb_pins];

            for (int i = 0 ; i < nb_pins ; i++)
                pins[i] = new Pin(Enum.GetName(typeof(PinsEnumT), i), signals[i], PullType.PullUp);
        }

        public abstract string familyname
        {
            get;
        }

        public string name
        {
            get { return _name; }
        }

        protected PinsGroup makePinsGroup(string name, int i1, int i2)
        {
            int k = Math.Abs(i1 - i2 + 1);
            Pin[] pinsGroup = new Pin[k];
            for (int i = i2 ; k >= 0 ; k--)
            {
                pinsGroup[k] = this.pins[i];
                i += i < i1 ? 1 : -1;
            }
            return new PinsGroup(name, pinsGroup);
        }
    }
}
