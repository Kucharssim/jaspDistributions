import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

import "./common" as LD

Form
{
	info: qsTr("Demonstration of the Normal (Gaussian) distribution")
	Section
	{
		expanded: true
		title: qsTr("Show Distribution")
		info: qsTr("Displays theoretical Normal distribution, given specified parameter values.")
		Group
		{
			Layout.columnSpan: 2
			DropDown
			{
				name: "parametrization"
				id:   parametrization
				indexDefaultValue: 0
				label: qsTr("Parameters")
				infoLabel: "<h3>" + label + "</h3>"
				values: [
					{ label: "μ, σ",  value: "sigma" },
					{ label: "μ, σ²", value: "sigma2" },
					{ label: "μ, τ",  value: "tau" },
				]
			}

			Group
			{
				columns: 2
				Text { text: qsTr("Mean:") }
				DoubleField{ name:  "mu"; label: qsTr("μ"); id: mu; negativeValues: true }
				DoubleField{ name: "sigma"; label: qsTr("σ"); id: sigma; negativeValues: false; enabled: parametrization.currentValue === "sigma"; defaultValue: 1}
				DoubleField{ name:  "sigma2"; label: qsTr("σ²"); id: sigma2; negativeValues: false; enabled: parametrization.currentValue === "sigma"; defaultValue: 1}
				DoubleField{ name:  "tau"; label: qsTr("τ"); id: sigma2; negativeValues: false; enabled: parametrization.currentValue === "sigma"; defaultValue: 1}
			}
		}

		Group
		{
			title: qsTr("Display")
			CheckBox{ name: "explanatoryText"; label: qsTr("Explanatory text")}
			CheckBox{ name: "parametersSupportMoments"; label: qsTr("Parameters, support, and moments")}
			CheckBox
			{ 
				name: "distributionTable" 
				label: qsTr("Distribution table")
				RadioButtonGroup
				{
					name: "distributionTableRangeBasedOn"
					title: qsTr("Range based on")
					id: distributionTableRangeBasedOn
					radioButtonsOnSameRow: true
					RadioButton { value: "quantile"; label: qsTr("Quantiles"); checked: true }
					RadioButton { value: "cdf";      label: qsTr("Cumulative probability"); checked: true }
				}

				DoubleField  { name: "distributionTableMin";    label: qsTr("Minimum"); id: distributionTableMin; min: distributionTableRangeBasedOn.value === "quantile" ? -Infinity : 0; max: distributionTableMax.value}
				DoubleField  { name: "distributionTableMax";    label: qsTr("Maximum"); id: distributionTableMax; max: distributionTableRangeBasedOn.value === "quantile" ?  Infinity : 1; min: distributionTableMin.value}
				IntegerField { name: "distributionTableLength"; label: qsTr("Number of values"); id: distributionTableLength}
			}
		}

		Group
		{
			title: qsTr("Plots")
			CheckBox{ name: "pdfPlot"; label: qsTr("Probability density function")}
			CheckBox{ name: "cdfPlot"; label: qsTr("Cumulative probability function")}
			CheckBox{ name: "qfPlot";  label: qsTr("Quantile function")}
		}
	}

	Section
	{
		title: qsTr("Generate and Display Data")

		CheckBox 
		{ 
			text: qsTr("Generate data from the theoretical distribution") 
			name: "generateData"
			id: generateData

			ComputedColumnField
			{
				name:					"generatedDataColumn"
				text:					qsTr("Column name")
				placeholderText:		qsTr("e.g., random data")
				fieldWidth:				120
				enabled:				generateData.checked
			}
		}

		VariablesForm
		{
			preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
			info: "<h3>" + qsTr("Get variable from data set: ") + "</h3>" + qsTr("Select a variable to display and fit with the theoretical distribution.")
			visible: true
			AvailableVariablesList { name: "allVariables" }
			AssignedVariablesList
			{
				name: "variable"; label: qsTr("Get variable from data set");
				singleVariable: true
			}
		}

		CheckBox
		{
			name: "dataDescriptives"
			label: qsTr("Descriptives")
			info: qsTr("Displays a descriptive table of the selected variable.")
			checked: true
		}

		CheckBox
		{
			name: "dataHistogram"
			label: qsTr("Histogram")
			into: qsTr("Display a histogram of the selected variable")
		}	
	}

	Section
	{
		title: qsTr("Estimate Parameters")

		CheckBox { name: "estimateParameters"; label: qsTr("Estimate parameters") }

		CheckBox { name: "besselsCorrection"; label: qsTr("Bessels correction")}

		CheckBox 
		{
			name: "parameterTable"
			label: qsTr("Parameter table")

			CheckBox
			{
				name: "parameterTableUncertainty"; label: qsTr("Quantify uncertainty"); childrenOnSameRow: true;
				CIField{ name: "parameterTableCiLevel" }
			}
		}
	}

	Section
	{
		title: qsTr("Assess fit")

		Group
		{
			title: qsTr("Plots")
			CheckBox 
			{ 
				name:	"histPlot"
				label:	qsTr("Histogram vs. theoretical pdf")
				info:	qsTr("Displays a histogram of the selected variable overlayed with the probability density function of the fitted distribution")
			}
			CheckBox 
			{ 
				name:	"qqPlot" 
				label:	qsTr("Q-Q plot") 
				info:	qsTr("Displays the quantile-quantile plot. The *x*-axis shows the theoretical quantiles of the data points under the fitted distribution, the *y*-axis shows the empirical quantiles of the selected variable.")
				CheckBox
				{
					name:	"qqPlotCi"
					label:	qsTr("Confidence interval")
					childrenOnSameRow:	true
					CIField{ name: "qqPlotCiLevel" }
				}
			}
			CheckBox 
			{ 
				name:	"ecdfPlot"
				label:	qsTr("Empirical vs. theoretical cdf")
				info:	qsTr("Displays an empirical cumulative distribution plot overlayed with the cumulative distribution function of the fitted distribution")
			}
			CheckBox
			{
				name:	"ppPlot"
				label:	qsTr("P-P plot")
				info:	qsTr("Displays the probability-probability plot. The *x*-axis shows the theoretical value of the cumulative density function of the data points under the fitted distribution, the *y*-axis shows the empirical percentiles of the selected variable.")
				CheckBox
				{
					name:	"ppPlotCi"
					label:	qsTr("Confidence interval")
					childrenOnSameRow:	true
					CIField{ name: "ppPlotCiLevel" }
				}
			}
		}

		Group
		{
			title: qsTr("Statistics")
			CheckBox
			{
				name:	"goodnessOfFit"
				label:	qsTr("Goodness-of-fit tests")
				info:	qsTr("Displays goodness of fit tests. The selection of tests is based on a distribution and whether its parameters are estimated from the data.")
				CheckBox
				{
					name: "goodnessOfFitBootstrap"
					label: qsTr("Bootstraped p-values from")
					childrenOnSameRow: true
					IntegerField
					{
						name: "goodnessOfFitBootstrapSamples"
					}
				}
			}

			CheckBox
			{
				name:	"informationCriteria"
				label:	qsTr("Information criteria")
				info:	qsTr("Displays the log-likelihood, AIC, and BIC of the distribution.")
			}

			CheckBox
			{
				name:	"momentsTable"
				label:	qsTr("First")
				info: qsTr("Displays a table with the raw and central moments of the selected variable and the moments of the distribution. Defaults to first 2 moments.")
				childrenOnSameRow: true
				IntegerField{name: "momentsTableNumber"; afterLabel: qsTr("moments"); defaultValue: 2; min: 1 }
			}
		}
		
	}
}