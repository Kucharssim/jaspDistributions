import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

DropDown
{
	name: "parametrization"
	indexDefaultValue: 0
	label: qsTr("Parameterization")
	infoLabel: "<h2>" + label + "</h2>"
	info: qsTr("Select parametrization of the distribution.")
}