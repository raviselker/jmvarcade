
// This file is an automatically generated and should not be edited

'use strict';

const options = [{"name":"data","type":"Data"},{"name":"letters","title":"Letters","type":"Variable"}];

const view = View.extend({

    events: [

	]

});

view.layout = ui.extend({

    label: "Hangman",
    type: "root",
    stage: 0, //0 - release, 1 - development, 2 - proposed
    controls: [
		{
			type: DefaultControls.VariableSupplier,
			persistentItems: false,
			stretchFactor: 1,
			controls: [
				{
					type: DefaultControls.VariableTargetListBox,
					name: "letters",
					label: "Letters",
					maxItemCount: 1,
					showColumnHeaders: false,
					fullRowSelect: true,
					columns: [
						{
							type: DefaultControls.ListItem.VariableLabel,
							name: "column1",
							label: "",
							stretchFactor: 1
						}
					]
				}
			]
		}
	]
});

module.exports = { view : view, options: options };
