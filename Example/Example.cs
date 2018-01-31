using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.SmallBasic.Library;
using Math = Microsoft.SmallBasic.Library.Math;

namespace Example
{
    class Program
    {
        public static Primitive Test { get; set; }

        private static Primitive _one;

        public static Primitive One
        {
            get => _one;
            set => SetOne(value);
        }
        private static void SetOne(Primitive value)
        {
            if (value.LessThanOrEqualTo(new Primitive(0)))
                throw new ArgumentOutOfRangeException(nameof(value));
            _one = value;
        }

        public static Primitive Two => One + 1;

        private static void Init_Test()
        {
            
        }

        static void Main(string[] args)
        {
            Test = new Primitive(0);
            //Test.M
            One = 1;

            var p = Math.Power(One, Two);
            TextWindow.WriteLine(Two);
            var x = One + Two;
            Console.WriteLine(Test.ToString());
        }
    }
}
