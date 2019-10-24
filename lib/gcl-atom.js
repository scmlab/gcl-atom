'use babel';

import GclAtomView from './gcl-atom-view';
import { CompositeDisposable } from 'atom';

export default {

  gclAtomView: null,
  modalPanel: null,
  subscriptions: null,

  activate(state) {
    this.gclAtomView = new GclAtomView(state.gclAtomViewState);
    this.modalPanel = atom.workspace.addModalPanel({
      item: this.gclAtomView.getElement(),
      visible: false
    });

    // Events subscribed to in atom's system can be easily cleaned up with a CompositeDisposable
    this.subscriptions = new CompositeDisposable();

    // Register command that toggles this view
    this.subscriptions.add(atom.commands.add('atom-workspace', {
      'gcl-atom:toggle': () => this.toggle()
    }));
  },

  deactivate() {
    this.modalPanel.destroy();
    this.subscriptions.dispose();
    this.gclAtomView.destroy();
  },

  serialize() {
    return {
      gclAtomViewState: this.gclAtomView.serialize()
    };
  },

  toggle() {
    console.log('GclAtom was toggled!');
    return (
      this.modalPanel.isVisible() ?
      this.modalPanel.hide() :
      this.modalPanel.show()
    );
  }

};
