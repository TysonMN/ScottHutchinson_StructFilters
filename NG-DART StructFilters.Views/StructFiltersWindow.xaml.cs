using System.Windows;
using System.Windows.Controls;

namespace NGDartStructFilters.Views {
    /// <summary>
    /// Interaction logic for StructFiltersWindow.xaml
    /// </summary>
    public partial class StructFiltersWindow : Window {
        public StructFiltersWindow() {
            /* This next line is needed to prevent a runtime exception
            * 'Could not load file or assembly 'Microsoft.Xaml.Behaviors...'
            * See https://github.com/microsoft/XamlBehaviorsWpf/issues/86
            */
            _ = new Microsoft.Xaml.Behaviors.EventTrigger {
                SourceName = "foo"
            };
            InitializeComponent();
        }
    }
}
