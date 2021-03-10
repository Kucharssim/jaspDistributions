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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets		1.0

Section
{
	property bool includeSE: true
	property bool includeCI: true
	property bool includeMCMC: false
	property var parameterLabels: []

	title: enabled ? qsTr("Estimate Parameters") : qsTr("Estimate Parameters") + " - " + qsTr("[requires a loaded data set]")
	columns: 2

	CheckBox
	{
		name: "methodMLE";      label: qsTr("Maximum likelihood"); id: methodMLE
		CheckBox
		{
			name: "outputEstimates"; label: qsTr("Estimates"); checked: true
			CheckBox{ name: "outputSE"; label: qsTr("Std. error"); checked: false; visible: includeSE }
			CheckBox
			{
				name: "ciInterval"; label: qsTr("Confidence interval"); childrenOnSameRow: true; visible: includeCI
				PercentField{ name: "ciIntervalInterval"; label: ""; defaultValue: 95 }
			}
		}
	}

	CheckBox
	{
		name: "methodMCMC"; label: qsTr("Markov Chain Monte Carlo"); id: methodMCMC

		Layout.columnSpan: 2
		columns: 2
		Group
		{
			title: qsTr("Priors")
			Repeater
			{
				model: parameterLabels
				TextField { label: modelData + " ~ "; name: "prior" + index }
			}
		}

		Group
		{
			title: qsTr("Options")
			PercentField { name: "credibleIntervalInterval"; label: qsTr("Credible interval"); defaultValue: 95 }
		}

		Group
		{
			title: qsTr("MCMC options")
			IntegerField
			{
				name: "noSamples"
				label: qsTr("No. samples")
				defaultValue: 2e3
				min: 10
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "noBurnin"
				label: qsTr("No. burnin samples")
				defaultValue: 500
				min: 1
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "noThinning"
				label: qsTr("Thinning")
				defaultValue: 1
				min: 1
				max: 1e9
				fieldWidth: 100
			}
			IntegerField
			{
				name: "noChains"
				label: qsTr("No. chains")
				defaultValue: 3
				min: 1
				max: 50
				fieldWidth: 100
			}
		}
	}
	HelpButton
	{
		toolTip: qsTr("Click to learn more about setting the priors.")
		helpPage: "miscellaneous/priors"
	}
}
