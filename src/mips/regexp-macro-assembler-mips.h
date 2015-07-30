// Copyright 2011 the V8 project authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file.


#ifndef V8_MIPS_REGEXP_MACRO_ASSEMBLER_MIPS_H_
#define V8_MIPS_REGEXP_MACRO_ASSEMBLER_MIPS_H_

#include "src/macro-assembler.h"
#include "src/mips/assembler-mips-inl.h"
#include "src/mips/assembler-mips.h"
#include "src/mips/macro-assembler-mips.h"

namespace v8 {
namespace internal {

#ifndef V8_INTERPRETED_REGEXP

class DelayedRegExpMacroAssemblerMIPS {
 public:
  DelayedRegExpMacroAssemblerMIPS(MacroAssembler* masm)
    : state(None),
      masm_(masm),
      jump_label_(NULL) {
  }
  ~DelayedRegExpMacroAssemblerMIPS() {
    DCHECK(state == None);
  }
  void Jump(Label * l) {
    EmitPending();
    DCHECK(state == None);

    if (l->is_bound()) {
      masm_->jmp(l);
    } else {
      jump_label_ = l;
      state = Jumped;
    }
 }
  void Bind(Label * l) {
    if (state != Jumped && state != None) {
      EmitPending();
    }

    DCHECK(state == Jumped || state == None);
    if (l == jump_label_ && state == Jumped) {
      masm_->bind(l);
      state = None;
    } else {
      EmitPending();
      DCHECK(state == None);
      masm_->bind(l);
    }
    jump_label_ = NULL;
  }
  void EmitPending() {
    if (state == None) {
    } else if (state == Jumped) {
      EmitPendingJump();  
    } else if (state == LoadedCharacter || state == CheckedNotCharacter) {
      EmitPendingLoadCheck();
    }
    DCHECK(state == None);
  }

  void LoadCurrentCharacterUnchecked(Register current_character, 
      Register current_input_offset, 
      Register end_of_input_address, 
      int cp_offset, 
      int characters, 
      NativeRegExpMacroAssembler::Mode mode, 
      int char_size) {
    bool emitEmmediately = false;

    DCHECK(characters == 1);

    if (state == Jumped) {
      EmitPending();
    }

    DCHECK(state == None || state == LoadedCharacter || state == CheckedNotCharacter);

    if (state == None && characters == 1) {
      cp_offset_start_ = cp_offset_ = cp_offset;
      mode_ = mode;
      state = LoadedCharacter;
      current_character_ = current_character;
      current_input_offset_ = current_input_offset;
      end_of_input_address_ = end_of_input_address;
      char_size_ = char_size;
    } else if (state == CheckedNotCharacter && characters == 1 
             && mode_ == mode 
             && current_character_.code() == current_character.code()
             && current_input_offset_.code() == current_input_offset.code()
             && end_of_input_address_.code() == end_of_input_address.code()
             && char_size_ == char_size) {
      if (cp_offset_ == (cp_offset + 1)) {
        cp_offset_ = cp_offset;
        state = LoadedCharacter;
      } else {
        emitEmmediately = true;
      }
    } else {
      emitEmmediately = true;
    }

    if (emitEmmediately) {
      EmitPendingLoadCheck();
      Register offset = current_input_offset;
      if (cp_offset_ != 0) {
        masm_->Addu(t7, current_input_offset, Operand(cp_offset * char_size));
        offset = t7;
      }
      masm_->Addu(t5, end_of_input_address, Operand(offset));
      if (mode == NativeRegExpMacroAssembler::LATIN1) {
        masm_->lbu(current_character, MemOperand(t5, 0));
      } else if (mode == NativeRegExpMacroAssembler::UC16) {
        masm_->lhu(current_character, MemOperand(t5, 0));
      }
    }

  }

  void CheckNotCharacter(Register current_character, uint32_t c, Label* on_not_equal) {
    bool emitImmediately = false;

    if (state == Jumped) {
      EmitPending();
    }

    DCHECK(on_not_equal != NULL);

    if (state == LoadedCharacter && cp_offset_start_ == cp_offset_ && current_character_.code() == current_character.code()) {
      c_start_ = c_ = c;
      state = CheckedNotCharacter;
      on_not_equal_ = on_not_equal;
    } else if (state == LoadedCharacter && on_not_equal == on_not_equal_) {
      if (c_ == (c + 1)) {
        c_ = c;
        state = CheckedNotCharacter;
      } else {
        emitImmediately = true;
      }
    } else {
      emitImmediately = true;
    }

    if (emitImmediately) {
      EmitPendingLoadCheck();
      masm_->Branch(on_not_equal, ne, current_character, Operand(c));
    }
  }
 
 private:

  enum OperationState {
    None,
    Jumped,
    LoadedCharacter,
    CheckedNotCharacter,
  };

  inline void EmitPendingLoadCheck() {
    DCHECK(state == None || state == LoadedCharacter || state == CheckedNotCharacter);

    if (state == CheckedNotCharacter) {
      uint32_t c;
      int cp_offset;
      c = c_start_;
      cp_offset = cp_offset_start_;
      while (cp_offset >= cp_offset_) {
        if ((cp_offset - 4) >= cp_offset_) {
          EmitLoadCheck4Character(cp_offset, c);
          c -= 4;
          cp_offset -= 4;
        }
        else if ((cp_offset - 3) >= cp_offset_) {
          EmitLoadCheck3Character(cp_offset, c);
          c -= 3;
          cp_offset -= 3;
        }
        if ((cp_offset - 2) >= cp_offset_) {
          EmitLoadCheck2Character(cp_offset, c);
          c -= 2;
          cp_offset -= 2;
        } else {
          EmitLoadCheck1Character(cp_offset, c);
          c--;
          cp_offset--;
        }
      }
    } else if (state == LoadedCharacter) {
      uint32_t c;
      int cp_offset;
      for (c = c_start_, cp_offset = cp_offset_start_; cp_offset > cp_offset_; cp_offset--, c--) {
        EmitLoadCheck1Character(cp_offset, c);
      }
      // Adding the last missing load character
      Register offset = current_input_offset_;
      if (cp_offset_ != 0) {
        masm_->Addu(t7, current_input_offset_, Operand(cp_offset_ * char_size_));
        offset = t7;
      }
      masm_->Addu(t5, end_of_input_address_, Operand(offset));
      if (mode_ == NativeRegExpMacroAssembler::LATIN1) {
        masm_->lbu(current_character_, MemOperand(t5, 0));
      } else if (mode_ == NativeRegExpMacroAssembler::UC16) {
        masm_->lhu(current_character_, MemOperand(t5, 0));
      }
    }

    state = None;
  }

  inline void EmitPendingJump() {
    DCHECK(state == Jumped || state == None);
    if (state == Jumped) {
      DCHECK(jump_label_ != NULL);
      masm_->jmp(jump_label_);
      jump_label_ = NULL;
      state = None;
    }
  }

  void EmitLoadCheck1Character(int cp_offset, uint32_t c) {
    Register offset = current_input_offset_;
    if (cp_offset != 0) {
      masm_->Addu(t7, current_input_offset_, Operand(cp_offset * char_size_));
      offset = t7;
    }
    masm_->Addu(t5, end_of_input_address_, Operand(offset));
    if (mode_ == NativeRegExpMacroAssembler::LATIN1) {
      masm_->lbu(current_character_, MemOperand(t5, 0));
    } else if (mode_ == NativeRegExpMacroAssembler::UC16) {
      masm_->lhu(current_character_, MemOperand(t5, 0));
    }

    masm_->Branch(on_not_equal_, ne, current_character_, Operand(c));
  }

  void EmitLoadCheck2Character(int cp_offset, uint32_t c) {
    Register offset = current_input_offset_;
    if (cp_offset != 1) {
      masm_->Addu(t7, current_input_offset_, Operand((cp_offset - 1) * char_size_));
      offset = t7;
    }
    masm_->Addu(t5, end_of_input_address_, Operand(offset));
    if (mode_ == NativeRegExpMacroAssembler::LATIN1) {
      //masm_->lbu(current_character_, MemOperand(t5, 0));
      masm_->lwr(t7, MemOperand(t5, 0));
      masm_->lwl(t7, MemOperand(t5, 3));
      masm_->Ext(current_character_, t7, 8, 8);
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c), USE_DELAY_SLOT);
      masm_->Ext(current_character_, t7, 0, 8);
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c - 1));

    } else if (mode_ == NativeRegExpMacroAssembler::UC16) {
      DCHECK(false);
      masm_->lhu(current_character_, MemOperand(t5, 0));
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c));
    }
  }

  void EmitLoadCheck3Character(int cp_offset, uint32_t c) {
    Register offset = current_input_offset_;
    if (cp_offset != 2) {
      masm_->Addu(t7, current_input_offset_, Operand((cp_offset - 2) * char_size_));
      offset = t7;
    }
    masm_->Addu(t5, end_of_input_address_, Operand(offset));
    if (mode_ == NativeRegExpMacroAssembler::LATIN1) {
      //masm_->lbu(current_character_, MemOperand(t5, 0));
      masm_->lwr(t7, MemOperand(t5, 0));
      masm_->lwl(t7, MemOperand(t5, 3));
      masm_->Ext(current_character_, t7, 16, 8);
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c), USE_DELAY_SLOT);
      masm_->Ext(current_character_, t7, 8, 8);
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c - 1), USE_DELAY_SLOT);
      masm_->Ext(current_character_, t7, 0, 8);
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c - 2));
    } else if (mode_ == NativeRegExpMacroAssembler::UC16) {
      DCHECK(false);
      masm_->lhu(current_character_, MemOperand(t5, 0));
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c));
    }
   
  }

  void EmitLoadCheck4Character(int cp_offset, uint32_t c) {
    Register offset = current_input_offset_;
    if (cp_offset != 3) {
      masm_->Addu(t7, current_input_offset_, Operand((cp_offset - 3) * char_size_));
      offset = t7;
    }
    masm_->Addu(t5, end_of_input_address_, Operand(offset));
    if (mode_ == NativeRegExpMacroAssembler::LATIN1) {
      //masm_->lbu(current_character_, MemOperand(t5, 0));
      masm_->lwr(t7, MemOperand(t5, 0));
      masm_->lwl(t7, MemOperand(t5, 3));
      masm_->Ext(current_character_, t7, 24, 8);
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c), USE_DELAY_SLOT);
      masm_->Ext(current_character_, t7, 16, 8);
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c - 1), USE_DELAY_SLOT);
      masm_->Ext(current_character_, t7, 8, 8);
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c - 2), USE_DELAY_SLOT);
      masm_->Ext(current_character_, t7, 0, 8);
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c - 3));
    } else if (mode_ == NativeRegExpMacroAssembler::UC16) {
      DCHECK(false);
      masm_->lhu(current_character_, MemOperand(t5, 0));
      masm_->Branch(on_not_equal_, ne, current_character_, Operand(c));
    }
   
  }

  OperationState state;

  MacroAssembler* masm_;
  Label * jump_label_;

  int cp_offset_start_;
  int cp_offset_;
  NativeRegExpMacroAssembler::Mode mode_;
  Register current_character_;
  Register current_input_offset_;
  Register end_of_input_address_;
  uint32_t c_start_;
  uint32_t c_;
  Label * on_not_equal_;
  int char_size_;
};

class RegExpMacroAssemblerMIPS: public NativeRegExpMacroAssembler {
 public:
  RegExpMacroAssemblerMIPS(Isolate* isolate, Zone* zone, Mode mode,
                           int registers_to_save);
  virtual ~RegExpMacroAssemblerMIPS();
  virtual int stack_limit_slack();
  virtual void AdvanceCurrentPosition(int by);
  virtual void AdvanceRegister(int reg, int by);
  virtual void Backtrack();
  virtual void Bind(Label* label);
  virtual void CheckAtStart(Label* on_at_start);
  virtual void CheckCharacter(uint32_t c, Label* on_equal);
  virtual void CheckCharacterAfterAnd(uint32_t c,
                                      uint32_t mask,
                                      Label* on_equal);
  virtual void CheckCharacterGT(uc16 limit, Label* on_greater);
  virtual void CheckCharacterLT(uc16 limit, Label* on_less);
  // A "greedy loop" is a loop that is both greedy and with a simple
  // body. It has a particularly simple implementation.
  virtual void CheckGreedyLoop(Label* on_tos_equals_current_position);
  virtual void CheckNotAtStart(Label* on_not_at_start);
  virtual void CheckNotBackReference(int start_reg, Label* on_no_match);
  virtual void CheckNotBackReferenceIgnoreCase(int start_reg,
                                               Label* on_no_match);
  virtual void CheckNotCharacter(uint32_t c, Label* on_not_equal);
  virtual void CheckNotCharacterAfterAnd(uint32_t c,
                                         uint32_t mask,
                                         Label* on_not_equal);
  virtual void CheckNotCharacterAfterMinusAnd(uc16 c,
                                              uc16 minus,
                                              uc16 mask,
                                              Label* on_not_equal);
  virtual void CheckCharacterInRange(uc16 from,
                                     uc16 to,
                                     Label* on_in_range);
  virtual void CheckCharacterNotInRange(uc16 from,
                                        uc16 to,
                                        Label* on_not_in_range);
  virtual void CheckBitInTable(Handle<ByteArray> table, Label* on_bit_set);

  // Checks whether the given offset from the current position is before
  // the end of the string.
  virtual void CheckPosition(int cp_offset, Label* on_outside_input);
  virtual bool CheckSpecialCharacterClass(uc16 type,
                                          Label* on_no_match);
  virtual void Fail();
  virtual Handle<HeapObject> GetCode(Handle<String> source);
  virtual void GoTo(Label* label);
  virtual void IfRegisterGE(int reg, int comparand, Label* if_ge);
  virtual void IfRegisterLT(int reg, int comparand, Label* if_lt);
  virtual void IfRegisterEqPos(int reg, Label* if_eq);
  virtual IrregexpImplementation Implementation();
  virtual void LoadCurrentCharacter(int cp_offset,
                                    Label* on_end_of_input,
                                    bool check_bounds = true,
                                    int characters = 1);
  virtual void PopCurrentPosition();
  virtual void PopRegister(int register_index);
  virtual void PushBacktrack(Label* label);
  virtual void PushCurrentPosition();
  virtual void PushRegister(int register_index,
                            StackCheckFlag check_stack_limit);
  virtual void ReadCurrentPositionFromRegister(int reg);
  virtual void ReadStackPointerFromRegister(int reg);
  virtual void SetCurrentPositionFromEnd(int by);
  virtual void SetRegister(int register_index, int to);
  virtual bool Succeed();
  virtual void WriteCurrentPositionToRegister(int reg, int cp_offset);
  virtual void ClearRegisters(int reg_from, int reg_to);
  virtual void WriteStackPointerToRegister(int reg);
  virtual bool CanReadUnaligned();

  // Called from RegExp if the stack-guard is triggered.
  // If the code object is relocated, the return address is fixed before
  // returning.
  static int CheckStackGuardState(Address* return_address,
                                  Code* re_code,
                                  Address re_frame);

 private:
  // Offsets from frame_pointer() of function parameters and stored registers.
  static const int kFramePointer = 0;

  // Above the frame pointer - Stored registers and stack passed parameters.
  // Registers s0 to s7, fp, and ra.
  static const int kStoredRegisters = kFramePointer;
  // Return address (stored from link register, read into pc on return).
  static const int kReturnAddress = kStoredRegisters + 9 * kPointerSize;
  static const int kSecondaryReturnAddress = kReturnAddress + kPointerSize;
  // Stack frame header.
  static const int kStackFrameHeader = kReturnAddress + kPointerSize;
  // Stack parameters placed by caller.
  static const int kRegisterOutput = kStackFrameHeader + 20;
  static const int kNumOutputRegisters = kRegisterOutput + kPointerSize;
  static const int kStackHighEnd = kNumOutputRegisters + kPointerSize;
  static const int kDirectCall = kStackHighEnd + kPointerSize;
  static const int kIsolate = kDirectCall + kPointerSize;

  // Below the frame pointer.
  // Register parameters stored by setup code.
  static const int kInputEnd = kFramePointer - kPointerSize;
  static const int kInputStart = kInputEnd - kPointerSize;
  static const int kStartIndex = kInputStart - kPointerSize;
  static const int kInputString = kStartIndex - kPointerSize;
  // When adding local variables remember to push space for them in
  // the frame in GetCode.
  static const int kSuccessfulCaptures = kInputString - kPointerSize;
  static const int kInputStartMinusOne = kSuccessfulCaptures - kPointerSize;
  // First register address. Following registers are below it on the stack.
  static const int kRegisterZero = kInputStartMinusOne - kPointerSize;

  // Initial size of code buffer.
  static const size_t kRegExpCodeSize = 1024;

  // Load a number of characters at the given offset from the
  // current position, into the current-character register.
  void LoadCurrentCharacterUnchecked(int cp_offset, int character_count);

  // Check whether preemption has been requested.
  void CheckPreemption();

  // Check whether we are exceeding the stack limit on the backtrack stack.
  void CheckStackLimit();

  // Generate code which is used by CheckStackLimit
  void GenerateStackLimitCheck();

  // Generate a call to CheckStackGuardState.
  void CallCheckStackGuardState(Register scratch);

  // The ebp-relative location of a regexp register.
  MemOperand register_location(int register_index);

  // Register holding the current input position as negative offset from
  // the end of the string.
  inline Register current_input_offset() { return t2; }

  // The register containing the current character after LoadCurrentCharacter.
  inline Register current_character() { return t3; }

  // Register holding address of the end of the input string.
  inline Register end_of_input_address() { return t6; }

  // Register holding the frame address. Local variables, parameters and
  // regexp registers are addressed relative to this.
  inline Register frame_pointer() { return fp; }

  // The register containing the backtrack stack top. Provides a meaningful
  // name to the register.
  inline Register backtrack_stackpointer() { return t4; }

  // Register holding pointer to the current code object.
  inline Register code_pointer() { return t1; }

  // Byte size of chars in the string to match (decided by the Mode argument).
  inline int char_size() { return static_cast<int>(mode_); }

  // Equivalent to a conditional branch to the label, unless the label
  // is NULL, in which case it is a conditional Backtrack.
  void BranchOrBacktrack(Label* to,
                         Condition condition,
                         Register rs,
                         const Operand& rt);

  // Call and return internally in the generated code in a way that
  // is GC-safe (i.e., doesn't leave absolute code addresses on the stack)
  inline void SafeCall(Label* to,
                       Condition cond,
                       Register rs,
                       const Operand& rt);
  inline void SafeReturn();
  inline void SafeCallTarget(Label* name);

  // Pushes the value of a register on the backtrack stack. Decrements the
  // stack pointer by a word size and stores the register's value there.
  inline void Push(Register source);

  // Pops a value from the backtrack stack. Reads the word at the stack pointer
  // and increments it by a word size.
  inline void Pop(Register target);

  Isolate* isolate() const { return masm_->isolate(); }

  MacroAssembler* masm_;

  // Which mode to generate code for (Latin1 or UC16).
  Mode mode_;

  // One greater than maximal register index actually used.
  int num_registers_;

  // Number of registers to output at the end (the saved registers
  // are always 0..num_saved_registers_-1).
  int num_saved_registers_;

  // Labels used internally.
  Label entry_label_;
  Label start_label_;
  Label success_label_;
  Label backtrack_label_;
  Label exit_label_;
  Label check_preempt_label_;
  Label stack_overflow_label_;
  Label internal_failure_label_;
  Label check_limit_store_label_;

  Label* check_limit_label_;
  static const int kGenerateStackCheckEvery = 300;
  int generate_stack_check_counter_;

  DelayedRegExpMacroAssemblerMIPS delayed_masm_;
};

#endif  // V8_INTERPRETED_REGEXP


}}  // namespace v8::internal

#endif  // V8_MIPS_REGEXP_MACRO_ASSEMBLER_MIPS_H_
