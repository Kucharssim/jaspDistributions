//
// Copyright (C) 2013-2018 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import "./common" as LD

Form
{
	Section
	{
		expanded: true
		title: qsTr("Show Distribution")
		Group
		{
			Layout.columnSpan: 2
			DropDown
			{
				name: "parametrization"
				id:   parametrization
				indexDefaultValue: 0
				label: qsTr("Parameters")
				values: [
					{ label: "k, p",  value: "prob"},
					{ label: "φ, μ",  value: "mean" }
				]
				visible: true
			}

			Group
			{
				columns: 2
				Text { text: qsTr("Probability of Zero process:") }
				DoubleField{ name: "pi"; label: qsTr("π"); id: pi; min: 0; max: 1; defaultValue: 0.25 }
				Text{ text: [qsTr("Number of successes:"), qsTr("Dispersion:")][parametrization.currentIndex] }
				DoubleField
				{
					name: "size"; label: ["k", "φ"][parametrization.currentIndex]; id: size;
					defaultValue: 5; negativeValues: false
				}
				Text{ text: [qsTr("Probability of success:"), qsTr("Mean:")][parametrization.currentIndex] }
				DoubleField
				{
					name:  "par"; label: ["p", "μ"][parametrization.currentIndex]; id: par
					defaultValue: [0.5, 5][parametrization.currentIndex]
					min: 0; max: [1, Infinity][parametrization.currentIndex]
				}
			}

		}
		Group
		{
			title: qsTr("Display")
			CheckBox{ label: qsTr("Explanatory text"); name: "explanatoryText"}
			CheckBox{ label: qsTr("Parameters, support, and moments"); name: "parsSupportMoments" }
			CheckBox{ label: qsTr("Formulas"); name: "formulas"; visible: false}
			CheckBox{ label: qsTr("Probability mass function"); id: plotPMF; name: "plotPMF"; checked: true }
			CheckBox{ label: qsTr("Cumulative distribution function"); id: plotCMF; name: "plotCMF"; checked: false }
		}

		Group
		{
			title: qsTr("Options")
			enabled: plotPMF.checked || plotCMF.checked

			Row
			{
				spacing: jaspTheme.columnGroupSpacing
				DoubleField
				{
					name: "min_x"; label: qsTr("Range of x from"); id: min_x;
					defaultValue: 0; min: 0; max: parseFloat(max_x.value)
				}
				DoubleField
				{
					name: "max_x"; label: qsTr("to"); id: max_x;
					defaultValue: 10; min: parseFloat(min_x.value)
				}
			}

			Group
			{
				title: qsTr("Highlight")
				Group
				{
					columns: 2
					CheckBox{ name: "highlightDensity"; label: qsTr("Mass"); id: highlightDensity }
					CheckBox{ name: "highlightProbability"; label: qsTr("Cumulative Probability"); id: highlightProbability }
				}
				Row
				{
					spacing: jaspTheme.columnGroupSpacing
					IntegerField
					{
						name: "min"; label: qsTr("Interval"); afterLabel: qsTr("≤ X ≤"); id: min;
						negativeValues: false; defaultValue: 0; max: parseInt(max.value)
					}
					IntegerField
					{
						name: "max"; label: ""; id: max;
						min: parseInt(min.value); defaultValue: 5
					}
				}
			}
		}
	}

	LD.LDGenerateDisplayData
	{
		distributionName		: "Zero-inflated NBinomial"
		formula					: "π = " + pi.value + [", k = ", ", φ = "][parametrization.currentIndex] + size.value + [", p = ", ", μ = "][parametrization.currentIndex] + par.value
		histogramIsBarPlot		: true
		allowOnlyScaleColumns	: true
		suggestScaleColumns		: true
		enabled					: mainWindow.dataAvailable
	}

	LD.LDEstimateParameters { enabled: mainWindow.dataAvailable }

	LD.LDAssessFit { enabled: mainWindow.dataAvailable; distributionType: "counts" }
}
