using System;
using System.Net;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Documents;
using System.Windows.Ink;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Shapes;
using Microsoft.Silverlight.Testing;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using FSHP.Dom.Nodes;

namespace FSHP.Test
{
    [TestClass]
    public class DomTest
    {
        [TestMethod]
        public void NodeCreationTest()
        {
            var document = new Document();
            Assert.Equals("UTF-8", document.Charset);
            var element = document.CreateElement("elem1");
            Assert.Equals("elem1", element.LocalName);
            var text = document.CreateTextNode("text node");
            Assert.Equals("text node", text.Data);
            var comment = document.CreateComment("comment node");
            Assert.Equals("comment node", comment.Data);
        }

        [TestMethod]
        public void ChildNestingTest()
        {
            var document = new Document();
            var docElem = document.CreateElement("docElem");
            var comment = document.CreateComment("commentElem");
            document.AppendChild(comment);
            document.AppendChild(docElem);
            Assert.Equals(2, document.ChildNodes.Length);
        }
    }
}
